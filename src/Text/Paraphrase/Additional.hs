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

import           Control.Applicative
import qualified Data.ListLike       as LL

-- -----------------------------------------------------------------------------

-- | As with 'get', but require at least @n@ tokens in the input we
--   receive.
getAtLeast :: (ParseInput s) => Int -> Parser e s s
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
getAtLeast' :: (ParseInput s) => Int -> ParseState e s
               -> (Failure e s   r -> Success e s s r -> Result e s   r)
getAtLeast' !n pSt fl sc = runP (needMoreInput *> go n) pSt fl sc
  where
    go !n' = P $ \ pSt' fl' sc' ->
      if checkLength n' (input pSt')
         then sc' pSt' (input pSt')
         else runP (needMoreInput *> go n') pSt' fl' sc'

-- | Request more input.
needMoreInput :: (ParseInput s) => Parser e s ()
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
               ParseState e s
               -> (ParseState e s -> Result e s r) -- Failure case
               -> (ParseState e s -> Result e s r) -- Success case
               -> Result e s r
requestInput pSt fl sc = Partial partialLog $ \ s ->
  if LL.null s
     then fl (pSt { more = Complete })
     else sc (pSt { input = input pSt `appendStream` s
                  , add   = add   pSt `LL.append`    s
                  })
  where
    partialLog = createFinalLog (mergedLog pSt) AwaitingInput (input pSt)
