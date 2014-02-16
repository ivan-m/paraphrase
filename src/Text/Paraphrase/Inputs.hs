{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}
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

-- -----------------------------------------------------------------------------

-- | The types which we know how to manipulate at a low-level.  This
--   class defines the minimum that is required to use all parser
--   combinators.
--
--   Please note that not /all/ combinators require the input source
--   to be an instance of this class; this is to keep the types as
--   generic as possible.
--
--   Unless you're defining a new input source, you probably do not
--   need to examine the methods of this class.
class (Monoid s) => ParseInput s where

  -- | The elements of this input type.
  type Token s

  -- | Obtain the first token in the input.  Only used when the input
  --   isn't empty, honest!
  inputHead :: s -> Token s

  -- | Return all but the first token of the input.  Only used when
  --   the input isn't empty, honest!
  inputTail :: s -> s

  -- | Is the input empty?
  isEmpty :: s -> Bool
  isEmpty = not . (`lengthAtLeast` 1)

  -- | Do we have at least @n@ tokens available?
  lengthAtLeast :: s -> Int -> Bool

  -- | Split the stream where the predicate is no longer satisfied
  --   (that is, the @fst@ component contains the largest possible
  --   prefix where all values satisfy the predicate, and the @snd@
  --   component contains the latter).
  breakWhen :: (Token s -> Bool) -> s -> (s,s)

instance ParseInput [a] where
  type Token [a] = a

  inputHead = head

  inputTail = tail

  isEmpty = null

  lengthAtLeast as n = not . null . drop (n-1) $ as
  {-# INLINE lengthAtLeast #-}

  breakWhen = span

instance ParseInput SB.ByteString where
  type Token SB.ByteString = Word8

  inputHead = SB.unsafeHead

  inputTail = SB.unsafeTail

  isEmpty = SB.null

  -- length is O(1)
  lengthAtLeast bs n = SB.length bs >= n
  {-# INLINE lengthAtLeast #-}

  breakWhen = SB.span

instance ParseInput LB.ByteString where
  type Token LB.ByteString = Word8

  inputHead = LB.head

  inputTail = LB.tail

  isEmpty = LB.null

  -- length is O(n)
  lengthAtLeast bs n = LB.length bs >= fromIntegral n
  {-# INLINE lengthAtLeast #-}

  breakWhen = LB.span

instance ParseInput ST.Text where
  type Token ST.Text = Char

  inputHead = ST.unsafeHead

  inputTail = ST.unsafeTail

  isEmpty = ST.null

  -- We do @`quot` 2@ because UTF-16 (which Text is implemented with)
  -- code points are either 1 or 2 Word16 values.  As such do this
  -- O(1) test first in case it suffices before we do the O(n) case
  -- for the real length.
  lengthAtLeast t n = (ST.lengthWord16 t `quot` 2) >= n || ST.length t >= n
  {-# INLINE lengthAtLeast #-}

  breakWhen = ST.span

instance ParseInput LT.Text where
  type Token LT.Text = Char

  inputHead = LT.head

  inputTail = LT.tail

  isEmpty = LT.null

  -- Doesn't seem to be any real alternative but to do the O(n)
  -- length.
  lengthAtLeast t n = LT.length t >= fromIntegral n
  {-# INLINE lengthAtLeast #-}

  breakWhen = LT.span

newtype AsChar8 s = AsChar8 { unChar8 :: s }
                    deriving (Eq, Ord, Show, Read, IsString, Monoid, NFData)

instance (ParseInput s, Token s ~ Word8) => ParseInput (AsChar8 s) where
  type Token (AsChar8 s) = Char

  inputHead (AsChar8 s) = w2c $! inputHead s

  inputTail = AsChar8 . inputTail . unChar8

  isEmpty = isEmpty . unChar8

  lengthAtLeast s = lengthAtLeast (unChar8 s)
  {-# INLINE lengthAtLeast #-}

  breakWhen f = (AsChar8 *** AsChar8) . breakWhen (f . w2c) . unChar8
