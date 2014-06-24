{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, TypeFamilies #-}

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
  ( AsChar8(..)
  , Word8Input(..)
  ) where

import Text.Paraphrase.Inputs

import qualified Data.ByteString            as SB
import           Data.ByteString.Char8      ()
import           Data.ByteString.Internal   (w2c)
import qualified Data.ByteString.Lazy       as LB
import           Data.ByteString.Lazy.Char8 ()
import           Data.String                (IsString (..))
import           Data.Word                  (Word8)

import Control.Arrow   (second)
import Control.DeepSeq (NFData)
import Data.IsNull     (IsNull)
import Data.Monoid     (Monoid)

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
class (ParseInput s, Token s ~ Word8) => Word8Input s where
  toWord8List :: s -> [Word8]

instance Word8Input [Word8] where
  toWord8List = id

instance Word8Input SB.ByteString where
  toWord8List = SB.unpack

instance Word8Input LB.ByteString where
  toWord8List = LB.unpack

instance (Word8Input s) => TokenStream (AsChar8 s) where
  type Stream (AsChar8 s) = Stream s

  type Token (AsChar8 s) = Char

  prettyInput (AsChar8 s) = addPrettyInput ("Char8 representation", char8)
                                           (prettyInput s)
    where
      char8 = prettyValue . map w2c . toWord8List $ s

instance (Word8Input s) => ParseInput (AsChar8 s) where

  getStream = getStream . unChar8
  {-# INLINE getStream #-}

  fromStream = AsChar8 . fromStream
  {-# INLINE fromStream #-}

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

-- -----------------------------------------------------------------------------
