{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving,
             TypeFamilies, DefaultSignatures #-}
{- |
   Module      : Text.Paraphrase.Inputs
   Description : Defining possible inputs for parsing
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Text.Paraphrase.Inputs where

import qualified Data.ByteString            as SB
import           Data.ByteString.Char8      ()
import           Data.ByteString.Internal   (w2c)
import qualified Data.ByteString.Lazy       as LB
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Unsafe     as SB
import           Data.String                (IsString (..))
import qualified Data.Text                  as ST
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Unsafe           as ST
import           Data.Word                  (Word8)

import Control.Arrow   ((***))
import Control.DeepSeq (NFData)
import Data.Monoid
import Data.IsNull

-- -----------------------------------------------------------------------------

-- | This class distinguishes between three concepts:
--
--   * The input type provided to a parser @s@.
--
--   * The underlying values being parsed @Stream s@ (in many cases
--     @Stream s ~ s@).
--
--   * The individual elements which compose the input, @Token s@.
--
--   This class is separated from 'ParseInput' so as to be able to
--   provide a minimum requirement for some definitions.
class TokenStream s where

  -- | The actual input stream.  In many cases @Stream s ~ s@
  --   (especially for base values to be parsed); if this is the case
  --   then no definition is required..
  type Stream s
  type Stream s = s

  -- | The elements of this input type.
  type Token s

-- | The types which we know how to manipulate at a low-level.  This
--   class defines the minimum that is required to use all parser
--   combinators.
--
--   Unless you're defining a new input source, you probably do not
--   need to examine the methods of this class.
class (TokenStream s, Monoid (Stream s), IsNull (Stream s)) => ParseInput s where

  -- | The current stream stored in this input value.
  getStream :: s -> Stream s
  default getStream :: (Stream s ~ s) => s -> s
  getStream = id
  {-# INLINE getStream #-}

  -- | Create a new input value from the provided stream.  As such, a
  --   blank input value will be equivalent to @fromStream 'mempty'@.
  fromStream :: Stream s -> s
  default fromStream :: (Stream s ~ s) => s -> s
  fromStream = id
  {-# INLINE fromStream #-}

  -- | Add new @Stream@ value to the front.  Default provided for
  --   values that are their own Stream.
  prependStream :: Stream s -> s -> s
  default prependStream :: (Stream s ~ s) => s -> s -> s
  prependStream = (<>)
  {-# INLINE prependStream #-}

  -- | Add new @Stream@ value to the back.  Default provided for
  --   values that are their own Stream.
  appendStream :: s -> Stream s ->  s
  default appendStream :: (Stream s ~ s) => s -> s -> s
  appendStream = (<>)
  {-# INLINE appendStream #-}

  -- | Obtain the first token in the input.  Only used when the input
  --   isn't empty, honest!
  inputHead :: s -> Token s

  -- | Return all but the first token of the input.  Only used when
  --   the input isn't empty, honest!
  inputTail :: s -> s

  -- | Do we have at least @n@ tokens available?
  lengthAtLeast :: s -> Int -> Bool

  -- | Split the stream where the predicate is no longer satisfied
  --   (that is, the @fst@ component contains the largest possible
  --   prefix where all values satisfy the predicate, and the @snd@
  --   component contains the latter).
  breakWhen :: (Token s -> Bool) -> s -> (Stream s,s)

-- | Is the current state of the input empty?
isEmpty :: (ParseInput s) => s -> Bool
isEmpty = isNull . getStream
{-# INLINE isEmpty #-}

-- -----------------------------------------------------------------------------
-- Instances for various concrete types.

instance TokenStream [a] where
  type Token [a] = a

instance ParseInput [a] where

  inputHead = head
  {-# INLINE inputHead #-}

  inputTail = tail
  {-# INLINE inputTail #-}

  lengthAtLeast as n = not . null . drop (n-1) $ as
  {-# INLINE lengthAtLeast #-}

  breakWhen = span
  {-# INLINE breakWhen #-}

instance TokenStream SB.ByteString where
  type Token SB.ByteString = Word8

instance ParseInput SB.ByteString where

  inputHead = SB.unsafeHead
  {-# INLINE inputHead #-}

  inputTail = SB.unsafeTail
  {-# INLINE inputTail #-}

  -- length is O(1)
  lengthAtLeast bs n = SB.length bs >= n
  {-# INLINE lengthAtLeast #-}

  breakWhen = SB.span
  {-# INLINE breakWhen #-}

instance TokenStream LB.ByteString where
  type Token LB.ByteString = Word8

instance ParseInput LB.ByteString where

  inputHead = LB.head
  {-# INLINE inputHead #-}

  inputTail = LB.tail
  {-# INLINE inputTail #-}

  -- length is O(n)
  lengthAtLeast bs n = LB.length bs >= fromIntegral n
  {-# INLINE lengthAtLeast #-}

  breakWhen = LB.span
  {-# INLINE breakWhen #-}

instance TokenStream ST.Text where
  type Token ST.Text = Char

instance ParseInput ST.Text where

  inputHead = ST.unsafeHead
  {-# INLINE inputHead #-}

  inputTail = ST.unsafeTail
  {-# INLINE inputTail #-}

  -- We do @`quot` 2@ because UTF-16 (which Text is implemented with)
  -- code points are either 1 or 2 Word16 values.  As such do this
  -- O(1) test first in case it suffices before we do the O(n) case
  -- for the real length.
  lengthAtLeast t n = (ST.lengthWord16 t `quot` 2) >= n || ST.length t >= n
  {-# INLINE lengthAtLeast #-}

  breakWhen = ST.span
  {-# INLINE breakWhen #-}

instance TokenStream LT.Text where
  type Token LT.Text = Char

instance ParseInput LT.Text where

  inputHead = LT.head
  {-# INLINE inputHead #-}

  inputTail = LT.tail
  {-# INLINE inputTail #-}

  -- Doesn't seem to be any real alternative but to do the O(n)
  -- length.
  lengthAtLeast t n = LT.length t >= fromIntegral n
  {-# INLINE lengthAtLeast #-}

  breakWhen = LT.span
  {-# INLINE breakWhen #-}

-- -----------------------------------------------------------------------------
-- How to treat Word8-based types as if they contained Char values.

-- | A wrapper to be able to parse 'Word8'-based types
--   (e.g. 'SB.ByteString') as if they actually contained
--   Latin1-encoded 'Char' values.
--
--   Note that the 'Stream' is itself rather than the raw type
--   underneath; this is primarily so that when printing error
--   messages the 'Char'-based representation is used rather than the
--   raw bytes.
--
--   As such, you will need to wrap any input (including additional
--   input) with the constructor.
--
--   This type must be applied directly to the actual type for it to
--   work.
newtype AsChar8 s = AsChar8 { unChar8 :: s }
                    deriving (Eq, Ord, Show, Read, IsString, Monoid, NFData, IsNull)

-- | For values that store 'Word8's.  The constraints are used more to
--   minimise the required constraints for 'AsChar8' instances than
--   because this typeclass needs/assumes them.
class (ParseInput s, Stream s ~ s, Token s ~ Word8) => Word8Input s where
  toWord8List :: s -> [Word8]

instance Word8Input [Word8] where
  toWord8List = id

instance Word8Input SB.ByteString where
  toWord8List = SB.unpack

instance Word8Input LB.ByteString where
  toWord8List = concatMap SB.unpack . LB.toChunks

instance (Word8Input s) => TokenStream (AsChar8 s) where
  type Stream (AsChar8 s) = AsChar8 s

  type Token (AsChar8 s) = Char

instance (Word8Input s) => ParseInput (AsChar8 s) where
  inputHead (AsChar8 s) = w2c $! inputHead s
  {-# INLINE inputHead #-}

  inputTail = AsChar8 . inputTail . unChar8
  {-# INLINE inputTail #-}

  lengthAtLeast s = lengthAtLeast (unChar8 s)
  {-# INLINE lengthAtLeast #-}

  breakWhen f = (AsChar8 *** AsChar8) . breakWhen (f . w2c) . unChar8
  {-# INLINE breakWhen #-}
