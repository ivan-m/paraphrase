{-# LANGUAGE BangPatterns #-}
{- |
   Module      : ComparseParsers
   Description : Benchmarking parser-combinator libraries
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main where

import           Control.Applicative
import           Control.DeepSeq       (NFData, deepseq)
import           Control.Exception     (evaluate)
import           Control.Monad         (replicateM)
import           Criterion.Main        (bench, bgroup, defaultMain, nf)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import           Data.Word             (Word8)


import qualified Data.Attoparsec.ByteString       as AB
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.ByteString.Lazy  as ABL
import qualified Data.Attoparsec.Text             as AT
import qualified Data.Attoparsec.Text.Lazy        as ATL

import qualified Text.Paraphrase as PP

import qualified CopyAttoparsec      as CA
import qualified CopyAttoparsecState as CS

-- -----------------------------------------------------------------------------

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
 where go xs = case splitAt k xs of
                 ([],_)  -> []
                 (y, ys) -> y : go ys

fromLazy :: BL.ByteString -> B.ByteString
fromLazy = B.concat . BL.toChunks

forceIO :: (NFData a) => a -> IO ()
forceIO = evaluate . (`deepseq` ())

forceMultiple :: (NFData b) => (a -> b) -> a -> IO ()
forceMultiple f = forceIO . replicateM 10000 f

main :: IO ()
main = do
  let s    = take 1024 . cycle $ ['a'..'z'] ++ ['A'..'Z']
      !b   = BC.pack s
      !bl  = BL.fromChunks . map BC.pack . chunksOf 4 $ s
      !bc  = PP.AsChar8 b
      !blc = PP.AsChar8 bl
      !t   = T.pack s
      !tl  = TL.fromChunks . map T.pack . chunksOf 4 $ s

  -- A suffix of @p@ means that the result is a @Partial@
  defaultMain
    [ bgroup "many-satisfy"

       [
         bgroup "attoparsec"
           [ bench "B"   $ nf (AB.parse     (many (AB.satisfy isChar))) b
           , bench "Bp"  $ nf (AB.parseOnly (many (AB.satisfy isChar))) b
           , bench "BC"  $ nf (AB.parse     (many (AC.satisfy isAlpha_ascii))) b
           , bench "BCp" $ nf (AB.parseOnly (many (AC.satisfy isAlpha_ascii))) b
           , bench "BL"  $ nf (ABL.parse    (many (AB.satisfy isChar))) bl
           , bench "BLC" $ nf (ABL.parse    (many (AC.satisfy isAlpha_ascii))) bl
           , bench "T"   $ nf (AT.parse     (many (AT.satisfy isAlpha_ascii))) t
           , bench "Tp"  $ nf (AT.parseOnly (many (AT.satisfy isAlpha_ascii))) t
           , bench "TL"  $ nf (ATL.parse    (many (AT.satisfy isAlpha_ascii))) tl
           ]

       , bgroup "copy-attoparsec"
           [ bench "B"  $ nf (CA.parse     (many (CA.satisfy isChar))) b
           , bench "Bp" $ nf (CA.parseOnly (many (CA.satisfy isChar))) b
           ]

       , bgroup "copy-attoparsec-state"
           [ bench "B"  $ nf (CS.parse     (many (CS.satisfy isChar))) b
           , bench "Bp" $ nf (CS.parseOnly (many (CS.satisfy isChar))) b
           ]

       , bgroup "paraphrase"
           [ bench "B"    $ nf (PP.parseInput (many (PP.satisfy isChar))) b
           , bench "Bp"   $ nf (parseOnly     (many (PP.satisfy isChar))) b
           , bench "BC"   $ nf (PP.parseInput (many (PP.satisfy isAlpha_ascii))) bc
           , bench "BCp"  $ nf (parseOnly     (many (PP.satisfy isAlpha_ascii))) bc
           , bench "BL"   $ nf (PP.parseInput (many (PP.satisfy isChar))) bl
           , bench "BLp"  $ nf (parseOnly     (many (PP.satisfy isChar))) bl
           , bench "BLC"  $ nf (PP.parseInput (many (PP.satisfy isAlpha_ascii))) blc
           , bench "BLCp" $ nf (parseOnly     (many (PP.satisfy isAlpha_ascii))) blc
           , bench "T"    $ nf (PP.parseInput (many (PP.satisfy isAlpha_ascii))) t
           , bench "Tp"   $ nf (parseOnly     (many (PP.satisfy isAlpha_ascii))) t
           , bench "TL"   $ nf (PP.parseInput (many (PP.satisfy isAlpha_ascii))) tl
           , bench "TLp"  $ nf (parseOnly     (many (PP.satisfy isAlpha_ascii))) tl
           ]
       ]

    , bgroup "many-next"
       [
         bgroup "attoparsec"
           [ bench "B"  $ nf (AB.parse     (many AB.anyWord8)) b
           , bench "Bp" $ nf (AB.parseOnly (many AB.anyWord8)) b
           , bench "BL" $ nf (ABL.parse    (many ABL.anyWord8)) bl
           , bench "T"  $ nf (AT.parse     (many AT.anyChar)) t
           , bench "Tp" $ nf (AT.parseOnly (many AT.anyChar)) t
           , bench "TL" $ nf (ATL.parse    (many ATL.anyChar)) tl
           ]

       , bgroup "copy-attoparsec"
           [ bench "B"  $ nf (CA.parse     (many CA.anyWord8)) b
           , bench "Bp" $ nf (CA.parseOnly (many CA.anyWord8)) b
           ]

       , bgroup "copy-attoparsec-state"
           [ bench "B"  $ nf (CS.parse     (many CS.anyWord8)) b
           , bench "Bp" $ nf (CS.parseOnly (many CS.anyWord8)) b
           ]

       , bgroup "paraphrase"
           [ bench "B"   $ nf (PP.parseInput (many PP.next)) b
           , bench "Bp"  $ nf (parseOnly     (many PP.next)) b
           , bench "BL"  $ nf (PP.parseInput (many PP.next)) bl
           , bench "BLp" $ nf (parseOnly     (many PP.next)) bl
           , bench "T"   $ nf (PP.parseInput (many PP.next)) t
           , bench "Tp"  $ nf (parseOnly     (many PP.next)) t
           , bench "TL"  $ nf (PP.parseInput (many PP.next)) tl
           , bench "TLp" $ nf (parseOnly     (many PP.next)) tl
           ]
       ]

    , bgroup "takeWhile"
        [
          bgroup "attoparsec"
            [ bench "B"   $ nf (AB.parse  (AB.takeWhile isChar)) b
            , bench "BC"  $ nf (AB.parse  (AC.takeWhile isAlpha_ascii)) b
            , bench "BL"  $ nf (ABL.parse (AB.takeWhile isChar)) bl
            , bench "BLC" $ nf (ABL.parse (AC.takeWhile isAlpha_ascii)) bl
            , bench "T"   $ nf (AT.parse  (AT.takeWhile isAlpha_ascii)) t
            , bench "TL"  $ nf (ATL.parse (AT.takeWhile isAlpha_ascii)) tl
            ]

        , bgroup "paraphrase"
            [ bench "B"   $ nf (PP.parseInput (PP.manySatisfy isChar)) b
            , bench "BC"  $ nf (PP.parseInput (PP.manySatisfy isAlpha_ascii)) bc
            , bench "BL"  $ nf (PP.parseInput (PP.manySatisfy isChar)) bl
            , bench "BLC" $ nf (PP.parseInput (PP.manySatisfy isAlpha_ascii)) blc
            , bench "T"   $ nf (PP.parseInput (PP.manySatisfy isAlpha_ascii)) t
            , bench "TL"  $ nf (PP.parseInput (PP.manySatisfy isAlpha_ascii)) tl
            ]
        ]
    ]

-- Just to benchmark like with like
parseOnly :: (PP.ParseInput s) => PP.Parser s a -> s -> PP.EitherResult s a
parseOnly p = fst . PP.runParser p
{-# INLINE parseOnly #-}

isAlpha_ascii :: Char -> Bool
isAlpha_ascii c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
{-# INLINE isAlpha_ascii #-}

isChar :: Word8 -> Bool
isChar c = (c >= 97 && c <= 122) || (c >= 65 && c <= 90)
{-# INLINE isChar #-}
