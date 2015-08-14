{-# LANGUAGE GeneralizedNewtypeDeriving, TypeFamilies #-}

{- |
   Module      : Text.Paraphrase.Wrappers
   Description : Potentially useful ParseInput wrappers
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module provides several wrappers around 'ParseInput' instances
   that might prove useful.

 -}
module Text.Paraphrase.Wrappers
  ( -- * Treating @Word8@ values as if they were @Char@s.
    AsChar8 (..)
  , Word8Input (..)
    -- * Counting the number of tokens consumed.
  , ConsumedTokens (..)
  ) where

import Text.Paraphrase.Inputs

import qualified Data.ByteString          as SB
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy     as LB
import           Data.String              (IsString (..))
import           Data.Word                (Word8)

import           Control.Arrow   (second)
import           Control.DeepSeq (NFData (..))
import qualified Data.ListLike   as LL

-- -----------------------------------------------------------------------------
-- How to treat Word8-based types as if they contained Char values.

-- | A wrapper to be able to parse 'Word8'-based types
--   (e.g. 'SB.ByteString') as if they actually contained
--   Latin1-encoded 'Char' values.
newtype AsChar8 s = AsChar8 { unChar8 :: s }
                    deriving (Eq, Ord, Show, Read, NFData)

-- | For values that store 'Word8's.  The constraints are used more to
--   minimise the required constraints for 'AsChar8' instances than
--   because this typeclass needs\/assumes them.
class (Show s, ParseInput s, Token s ~ Word8) => Word8Input s where
  toWord8List :: s -> [Word8]

  fromWord8List :: [Word8] -> s

instance (a ~ Word8) => Word8Input [a] where
  toWord8List = id

  fromWord8List = id

instance Word8Input SB.ByteString where
  toWord8List = SB.unpack

  fromWord8List = SB.pack

instance Word8Input LB.ByteString where
  toWord8List = LB.unpack

  fromWord8List = LB.pack

instance (Word8Input s) => PrettyValue (AsChar8 s) where
  prettyValue = prettyValue . map w2c . toWord8List . unChar8

-- | Assumes all 'Char's in the provided 'String' are only 8 bits.
instance (Word8Input s) => IsString (AsChar8 s) where
  fromString = AsChar8 . fromWord8List . map c2w

instance (Word8Input s) => TokenStream (AsChar8 s) where
  type Stream (AsChar8 s) = Stream s

  type Token (AsChar8 s) = Char

  prettyInput a = addPrettyInput ("Char8 representation", prettyValue a)
                                 (prettyInput (unChar8 a))

instance (Word8Input s) => ParseInput (AsChar8 s) where

  getStream = getStream . unChar8
  {-# INLINE getStream #-}

  fromStream = AsChar8 . fromStream
  {-# INLINE fromStream #-}

  extractCurrentStream = second AsChar8 . extractCurrentStream . unChar8
  {-# INLINE extractCurrentStream #-}

  prependStream s = AsChar8 . (prependStream s) . unChar8
  {-# INLINE prependStream #-}

  appendStream (AsChar8 inp) s = AsChar8 (inp `appendStream` s)
  {-# INLINE appendStream #-}

  inputHead (AsChar8 s) = w2c $! inputHead s
  {-# INLINE inputHead #-}

  inputTail = AsChar8 . inputTail . unChar8
  {-# INLINE inputTail #-}

  lengthAtLeast s = lengthAtLeast (unChar8 s)
  {-# INLINE lengthAtLeast #-}

  breakWhen f = (second AsChar8) . breakWhen (f . w2c) . unChar8
  {-# INLINE breakWhen #-}

  getStreamLength n = second AsChar8 . getStreamLength n . unChar8
  {-# INLINE getStreamLength #-}

-- -----------------------------------------------------------------------------

-- | Primarily aimed for better debugging support, this wrapper
--   indicates how many tokens have so far been consumed from the
--   input.
--
--   Production code should not use this wrapper as it affects
--   performance.
--
--   Note that this counter does not behave well with the 'reparse'
--   combinator (in that the provided stream decreases the count of
--   the number of tokens consumed).
data ConsumedTokens s = CT { consumed :: !Int
                           , cStream  :: !s
                           }
                      deriving (Eq, Ord, Show, Read)

createCT :: s -> ConsumedTokens s
createCT = CT 0

instance (ParseInput s) => TokenStream (ConsumedTokens s) where
  type Stream (ConsumedTokens s) = Stream s

  type Token  (ConsumedTokens s) = Token  s

  prettyInput (CT c s) = addPrettyInput ("Consumed Tokens", prettyValue c)
                                        (prettyInput s)

instance (ParseInput s) => ParseInput (ConsumedTokens s) where

  getStream = getStream . cStream
  {-# INLINE getStream #-}

  fromStream = createCT . fromStream
  {-# INLINE fromStream #-}

  extractCurrentStream (CT c s) =
    let (str,s') = extractCurrentStream s
        ct' = CT { consumed = c + LL.length str
                 , cStream  = s'
                 }
    in (str,ct')
  {-# INLINE extractCurrentStream #-}

  prependStream str (CT c s) = CT { consumed = c - LL.length str
                                  , cStream  = str `prependStream` s
                                  }
  {-# INLINE prependStream #-}

  appendStream ct s = ct { cStream = cStream ct `appendStream` s }
  {-# INLINE appendStream #-}

  inputHead = inputHead . cStream
  {-# INLINE inputHead #-}

  inputTail (CT c s) = CT { consumed = c + 1
                          , cStream  = inputTail s
                          }
  {-# INLINE inputTail #-}

  lengthAtLeast = lengthAtLeast . cStream
  {-# INLINE lengthAtLeast #-}

  breakWhen f (CT c s) =
    let (str, s') = breakWhen f s
        ct' = CT { consumed = c + LL.length str
                 , cStream  = s'
                 }
    in (str, ct')
  {-# INLINE breakWhen #-}

  getStreamLength n (CT c s) =
    let (str,s') = getStreamLength n s
        ct' = CT { consumed = c + n
                 , cStream  = s'
                 }
    in (str, ct')
  {-# INLINE getStreamLength #-}

instance (NFData s) => NFData (ConsumedTokens s) where
  rnf (CT c s) = rnf c `seq` rnf s

instance (IsString s) => IsString (ConsumedTokens s) where
  fromString = createCT . fromString
