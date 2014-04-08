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
import Data.Monoid

-- -----------------------------------------------------------------------------

-- | Make sure that there are at least @n@ 'Token's available.
needAtLeast :: (ParseInput s) => Int -> Parser s ()
needAtLeast !n = go
  where
    go = P $ \ pSt pl fl sc ->
      if lengthAtLeast (input pSt) n
         then sc pSt pl ()
         else runP (needMoreInput *> go) pSt pl fl sc
{-# INLINE needAtLeast #-}

-- | As with 'get', but require at least @n@ tokens in the input we
--   receive.
getAtLeast :: (ParseInput s) => Int -> Parser s s
getAtLeast !n = P $ \ pSt pl fl sc ->
     if checkLength n (input pSt)
        then sc pSt pl (input pSt)
        else getAtLeast' n pSt pl fl sc
{-# INLINE getAtLeast #-}

checkLength :: (ParseInput s) => Int -> s -> Bool
checkLength 1  = not . isEmpty
checkLength !n = (`lengthAtLeast` n)
{-# INLINE checkLength #-}

-- The un-common case is split off to avoid recursion in getAtLeast, so
-- that it can be inlined properly.
getAtLeast' :: (ParseInput s) => Int -> WithState s (Failure s   r -> Success s s r -> Result  s   r)
getAtLeast' !n pSt pl fl sc = runP (needMoreInput *> go n) pSt pl fl sc
  where
    go !n' = P $ \ pSt' pl' fl' sc' ->
      if checkLength n' (input pSt')
         then sc' pSt' pl' (input pSt')
         else runP (needMoreInput *> go n') pSt' pl' fl' sc'

-- | Request more input.
needMoreInput :: (ParseInput s) => Parser s ()
needMoreInput = P $ \ pSt pl fl sc ->
  if more pSt == Complete
     then fl pSt pl NoMoreInputExpected
     else let fl' pSt' pl' = fl pSt' pl' UnexpectedEndOfInput
              sc' pSt' pl' = sc pSt' pl' ()
          in requestInput pSt pl fl' sc'

-- | Construct a 'Partial' 'Result' with a continuation function that
--   will use the first provided function if it fails, and the second
--   if it succeeds.
requestInput :: (ParseInput s) =>
               WithState s
               (    WithState s (Result s r) -- Failure case
                 -> WithState s (Result s r) -- Success case
                 -> Result s r)
requestInput pSt pl fl sc = Partial partialLog $ \ s ->
  if isEmpty s
     then fl (pSt { more = Complete }) pl
     else sc (PSt (input pSt <> s) (add pSt <> s) Incomplete) pl
  where
    partialLog = createFinalLog pl AwaitingInput (input pSt)
