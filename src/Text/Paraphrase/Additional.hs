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
    go = P $ \ inp add mr pl fl sc ->
      if lengthAtLeast (unI inp) n
         then sc inp add mr pl ()
         else runP (needMoreInput *> go) inp add mr pl fl sc
{-# INLINE needAtLeast #-}

-- | As with 'get', but require at least @n@ tokens in the input we
--   receive.
getAtLeast :: (ParseInput s) => Int -> Parser s s
getAtLeast !n = P $ \ inp add mr pl fl sc ->
     if checkLength n (unI inp)
        then sc inp add mr pl (unI inp)
        else getAtLeast' n inp add mr pl fl sc
{-# INLINE getAtLeast #-}

checkLength :: (ParseInput s) => Int -> s -> Bool
checkLength 1  = not . isEmpty
checkLength !n = (`lengthAtLeast` n)
{-# INLINE checkLength #-}

-- The un-common case is split off to avoid recursion in getAtLeast, so
-- that it can be inlined properly.
getAtLeast' :: (ParseInput s) => Int -> WithState s (Failure s   r -> Success s s r -> Result  s   r)
getAtLeast' !n inp add mr pl fl sc = runP (needMoreInput *> go n) inp add mr pl fl sc
  where
    go !n' = P $ \ inp' add' mr' pl' fl' sc' ->
      if checkLength n' (unI inp')
         then sc' inp' add' mr' pl' (unI inp')
         else runP (needMoreInput *> go n') inp' add' mr' pl' fl' sc'

-- | Request more input.
needMoreInput :: (ParseInput s) => Parser s ()
needMoreInput = P $ \ inp add mr pl fl sc ->
  if mr == Complete
     then fl inp add mr pl NoMoreInputExpected
     else let fl' inp' add' mr' pl' = fl inp' add' mr' pl' UnexpectedEndOfInput
              sc' inp' add' mr' pl' = sc inp' add' mr' pl' ()
          in requestInput inp add mr pl fl' sc'

-- | Construct a 'Partial' 'Result' with a continuation function that
--   will use the first provided function if it fails, and the second
--   if it succeeds.
requestInput :: (ParseInput s) =>
               WithState s
               (    WithState s (Result s r) -- Failure case
                 -> WithState s (Result s r) -- Success case
                 -> Result s r)
requestInput inp add _mr pl fl sc = Partial partialLog $ \ s ->
  if isEmpty s
     then fl inp add Complete pl
     else sc (inp <> I s) (add <> A s) Incomplete pl
  where
    partialLog = createFinalLog pl AwaitingInput (unI inp)
