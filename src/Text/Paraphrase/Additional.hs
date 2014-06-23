{-# LANGUAGE BangPatterns #-}
{- |
   Module      : Text.Paraphrase.Additional
   Description : Functions for obtaining additional input
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Text.Paraphrase.Additional where

import Text.Paraphrase.Errors (ParseError (..), createFinalLog)
import Text.Paraphrase.Inputs
import Text.Paraphrase.Types

import Control.Applicative
import Data.IsNull
import Data.Monoid

-- -----------------------------------------------------------------------------

-- | Make sure that there are at least @n@ 'Token's available.
needAtLeast :: (ParseInput s) => Int -> Parser s ()
needAtLeast !n = go
  where
    go = P $ \ pSt fl sc ->
      if lengthAtLeast (input pSt) n
         then sc pSt ()
         else runP (needMoreInput *> go) pSt fl sc
{-# INLINE needAtLeast #-}

-- | As with 'get', but require at least @n@ tokens in the input we
--   receive.
getAtLeast :: (ParseInput s) => Int -> Parser s s
getAtLeast !n = P $ \ pSt fl sc ->
     if checkLength n (input pSt)
        then sc pSt (input pSt)
        else getAtLeast' n pSt fl sc
{-# INLINE getAtLeast #-}

checkLength :: (ParseInput s) => Int -> s -> Bool
checkLength 1  = not . isEmpty
checkLength !n = (`lengthAtLeast` n)
{-# INLINE checkLength #-}

-- The un-common case is split off to avoid recursion in getAtLeast, so
-- that it can be inlined properly.
getAtLeast' :: (ParseInput s) => Int -> ParseState s -> (Failure s   r -> Success s s r -> Result  s   r)
getAtLeast' !n pSt fl sc = runP (needMoreInput *> go n) pSt fl sc
  where
    go !n' = P $ \ pSt' fl' sc' ->
      if checkLength n' (input pSt')
         then sc' pSt' (input pSt')
         else runP (needMoreInput *> go n') pSt' fl' sc'

-- | Request more input.
needMoreInput :: (ParseInput s) => Parser s ()
needMoreInput = P $ \ pSt fl sc ->
  if more pSt == Complete
     then fl pSt NoMoreInputExpected
     else let fl' pSt' = fl pSt' UnexpectedEndOfInput
              sc' pSt' = sc pSt' ()
          in requestInput pSt fl' sc'

-- | Construct a 'Partial' 'Result' with a continuation function that
--   will use the first provided function if it fails, and the second
--   if it succeeds.
--
--   It is assumed that if this function is called, then @more pSt ==
--   Incomplete@.
requestInput :: (ParseInput s) =>
               ParseState s
               -> (ParseState s -> Result s r) -- Failure case
               -> (ParseState s -> Result s r) -- Success case
               -> Result s r
requestInput pSt fl sc = Partial partialLog $ \ s ->
  if isNull s
     then fl (pSt { more = Complete })
     else sc (pSt { input = input pSt `appendStream` s, add = add pSt <> s })
  where
    partialLog = createFinalLog (mergedLog pSt) AwaitingInput (input pSt)
