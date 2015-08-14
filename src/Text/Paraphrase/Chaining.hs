{-# LANGUAGE FlexibleContexts, StandaloneDeriving #-}

{- |
   Module      : Text.Paraphrase.Chaining
   Description : Implementation of chaining combinators
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Text.Paraphrase.Chaining where

import Text.Paraphrase.Errors
import Text.Paraphrase.Inputs
import Text.Paraphrase.Stack  (mergeStack)
import Text.Paraphrase.Types

import Control.Applicative
import Data.Bool           (bool)

-- -----------------------------------------------------------------------------

-- How to deal with cases where we want some info from the
-- sub-parser's log to be attached to the log of the outer parser
-- rather than just the failure?
--
-- Primarily for having cases of something that looks like:
--
-- > pa *> pb *> pa' *> pb *> pa' *> pb
--
-- (i.e. we keep alternating between the two).  We don't have access
-- to the internal ParseLog as running the parser discards that
-- information.

-- Also need to consider commitment.

-- Might need to use the parsers in Debug to deal with this: something
-- like @liftA3 isParserCommitted getCurrentLog pb@?

-- What about committing in pb, it returns a Partial, and then pa'
-- fails?

chainBy :: (ParseInput a, ParseInput b)
           => ChainConfig e a e' b
           -> Parser e' b c
              -- ^ Target parser.
           -> Parser e a (Stream b)
              -- ^ Initial source parser.
           -> Parser e a (Stream b)
              -- ^ Source parser to run if target parser requires more
              --   input.  If no more input is to be allowed, use
              --   @pure mempty@.
           -> Parser e a c
chainBy cc pb pa pa' = do
  inpA <- get
  (ec,bs') <- parseAndFeed (parseEither $ addInternals pb) pa pa'
  reintegrate inpA ec bs'
  where
    reintegrate inpA ec bs' = P $ \ pSt fl sc ->
      let fl pSt' e = undefined
      in undefined
      -- either (fl pSt . toE) (sc $ withB bs' pSt) ec
      where


        -- Committed failure.  Because it comes from a Failure value, we
        -- know it must have committed.

        commitWhen = undefined -- bool id commitState . (&&) (keepCommitted cc)

        bfl didCommit pe = commitWhen didCommit . onFailure cc pe inpA

        bsc pi pSt = commitWhen (wasCommitted pi)

-- Need to somehow get initial input... maybe start with "s <- get" ?

-- Wrap commitment around this

data ChainConfig e a e' b
  = CC { logMerge      :: ParsingErrors e' b -> a -> ParseLog e a -> ParseLog e a
       , withRemaining :: Stream b -> ParseState e a -> ParseState e a
       , keepCommitted :: Bool
       , onFailure     :: ParsingErrors e' b -> a -> ParseLog e a -> ParseLog e a
       }


-- Optionally also use the raw input for withRemaining...

-- Maybe generalise this a bit more so that pa and pa' don't need to
-- return the Stream but the raw input, so that we can add/remove
-- wrappers.

data ParseInternals e s a = PI { originalAnswer :: !a
                               , finalInput     :: !s
                               , finalLog       :: !(ParseLog e s)
                               , wasCommitted   :: !Bool
                               }

deriving instance (Eq   a, Eq   s, Eq   (ParseLog e s)) => Eq   (ParseInternals e s a)
deriving instance (Show a, Show s, Show (ParseLog e s)) => Show (ParseInternals e s a)

-- Note that since we want the value of the internals _after_ the
-- parse, we have to run the provided parser first.
addInternals :: Parser e s a -> Parser e s (ParseInternals e s a)
addInternals p = PI <$> p
                    <*> getInternalValue input
                    <*> getInternalValue (mergeStack . errLog)
                    <*> getInternalValue isCommitted

getInternalValue :: (ParseState e s -> a) -> Parser e s a
getInternalValue f = P $ \ pSt _fl sc -> sc pSt (f pSt)
{-# INLINE getInternalValue #-}

-- | Will always succeed (unless the parser is committed and fails
--   during a backtrack).
parseEither :: Parser e s a -> Parser e s (Either (ParsingErrors e s) a)
parseEither p = P $ \ pSt _fl sc ->
  let fl' pSt' e = sc pSt' (Left (createLogFrom pSt' e))
      sc' pSt' a = sc pSt' (Right a)
  in runP p pSt fl' sc'
{-# INLINE parseEither #-}
