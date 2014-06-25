{- |
   Module      : Text.Paraphrase.Pretty
   Description : Pretty-printing of values
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   To be able to pretty-print error logs, we need to be able to pretty-print

 -}
module Text.Paraphrase.Pretty where

import Text.PrettyPrint.HughesPJ

import qualified Data.ByteString      as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text            as ST
import qualified Data.Text.Lazy       as LT
import           Data.Word            (Word8)

-- -----------------------------------------------------------------------------

-- | A class used for pretty-printing values for error messages.
--
--   The 'Show' constraint isn't used directly (except for the default
--   implementation), but if you can't 'show' a value, how can you
--   have a pretty version of it?.
class (Show v) => PrettyValue v where
  prettyValue :: v -> Doc
  prettyValue = text . show

  prettyList :: [v] -> Doc
  prettyList = brackets . hcat . punctuate comma . map prettyValue

-- | Vertically pretty-print a list.  Useful for values that are
--   already "listful".
prettyStreamList :: (PrettyValue v) => [v] -> Doc
prettyStreamList []   = brackets empty
prettyStreamList list = vcat (zipWith (<+>) (lbrack : repeat comma)
                                            (map prettyValue list))
                        $+$ rbrack

-- -----------------------------------------------------------------------------
-- Instances

instance PrettyValue ()

instance PrettyValue Char where
  prettyValue = quotes . char

  prettyList = doubleQuotes . text

instance PrettyValue Int where
  prettyValue = int

instance PrettyValue Integer where
  prettyValue = integer

instance PrettyValue Word8 where
  prettyValue = integer . toInteger

instance PrettyValue Float where
  prettyValue = float

instance PrettyValue Double where
  prettyValue = double

instance PrettyValue Doc where
  prettyValue = id

instance (PrettyValue a) => PrettyValue [a] where
  prettyValue = prettyList

  prettyList = prettyStreamList

instance PrettyValue SB.ByteString where
  prettyValue = prettyList . SB.unpack

  prettyList = prettyStreamList

instance PrettyValue LB.ByteString where
  prettyValue = prettyList . map contents . LB.toChunks
    where
      contents = hcat . punctuate comma . map prettyValue . SB.unpack

  prettyList = prettyStreamList

instance PrettyValue ST.Text where
  prettyValue = prettyValue . ST.unpack

  prettyList = prettyStreamList

instance PrettyValue LT.Text where
  prettyValue = prettyValue . LT.unpack

  prettyList = prettyStreamList
