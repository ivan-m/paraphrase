{-# LANGUAGE BangPatterns #-}
{- |
   Module      : Text.Paraphrase.Additional
   Description : Functions for obtaining additional input
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Text.Paraphrase.Additional where

import Text.Paraphrase.Inputs
import Text.Paraphrase.Types

import Control.Applicative
import Data.Monoid

-- -----------------------------------------------------------------------------

-- | Make sure that there are at least @n@ 'Token's available.
needAtLeast :: (ParseInput s) => Int -> Parser s ()
needAtLeast !n = go
 where
   go = P $ \ !pSt fl sc ->
     if lengthAtLeast (input pSt) n
        then sc pSt ()
        else runP (needMoreInput *> go) pSt fl sc
{-# INLINE needAtLeast #-}

ensure :: (ParseInput s) => Int -> Parser s s
ensure !n = P $ \ !pSt fl sc ->
     if lengthAtLeast (input pSt) n
        then sc pSt (input pSt)
        else ensure' n pSt fl sc
{-# INLINE ensure #-}

-- The un-common case is split off to avoid recursion in ensure, so
-- that it can be inlined properly.
ensure' :: (ParseInput s) => Int -> ParseState s -> Failure s   r -> Success s s r -> Result  s   r
ensure' !n !pSt fl sc = runP (needMoreInput *> go n) pSt fl sc
 where
   go !n' = P $ \ !pSt' fl' sc' ->
     if lengthAtLeast (input pSt') n'
        then sc' pSt' (input pSt')
        else runP (needMoreInput *> go n') pSt' fl' sc'

-- | Request more input.
needMoreInput :: (ParseInput s) => Parser s ()
needMoreInput = P $ \ !pSt fl sc ->
 if more pSt == Complete
    then fl pSt UnexpectedEndOfInput
    else let fl' pSt' = fl pSt' UnexpectedEndOfInput
             sc' pSt' = sc pSt' ()
         in requestInput pSt fl' sc'

-- | Construct a 'Partial' 'Result' with a continuation function that
--   will use the first provided function if it fails, and the second
--   if it succeeds.
--
--   It is assumed that @more pSt == Incomplete@.
requestInput :: (ParseInput s) =>
               ParseState s
               -> (ParseState s -> (Result s r)) -- Failure case
               -> (ParseState s -> (Result s r)) -- Success case
               -> Result s r
requestInput !pSt fl sc = Partial (parseLog pSt) $ \ s ->
 if isEmpty s
    then fl (pSt { more = Complete })
    else sc (pSt { input = input pSt <> s, additional = additional pSt <> s})
