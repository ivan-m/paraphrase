{-# LANGUAGE BangPatterns #-}
{- |
   Module      : Text.Paraphrase
   Description : Experimental polyparse reimplementation
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is an experiment in creating a modern combinator-parsing
   library based upon the "Text.ParserCombinators.Poly.Base",
   etc. modules found in the
   <http://hackage.haskell.org/package/polyparse polyparse> library.

 -}
module Text.Paraphrase
       ( -- * The parser
         Parser
         -- ** Running parsers
       , parseInput
       , runParser
       , runParser'
         -- ** Parsing results
       , Result (..)
       , AdjustError
       , adjustError
       , resultToEither
       , EitherResult
         -- ** Parser input
       , ParseInput (..)
       , AsChar8(..)

         -- * Parser combinators
       , next
       , satisfy
       , satisfyWith
       , endOfInput
       , oneOf
       , bracket
       , reparse
         -- ** Commitment
       , commit
       , failBad
         -- ** Sequences
       , manySatisfy
       , someSatisfy
       , sepBy
       , sepBy1
       , bracketSep
       , manyFinally
       , manyFinally'
         -- ** Counting
       , exactly
       , upto
         -- ** Chaining parsers
       , chainParsers

         -- * Error reporting
         -- ** Convenience functions
         -- $stacktraces
       , failMessage
       , addStackTrace
       , addStackTraceBad
         -- ** Low-level error adjustment
       , adjustErr
       , adjustErrBad
       , indent
       , indentLine
       , allButFirstLine
       , oneOf'

         -- * Re-exported for extra combinators
       , module Control.Applicative
       ) where

import Text.Paraphrase.TextManipulation
import Text.Paraphrase.Types

import Control.Applicative
import Data.Monoid

-- -----------------------------------------------------------------------------
-- Commitment

-- | Prevent any backtracking from taking place by emphasising that
--   any failures from this parser are more severe than usual.
--
--   That is, @commit p1 \<|\> p2@ is equivalent to just @p1@ (though
--   also preventing any other usage of @'<|>'@ that might occur).
commit :: Parser s a -> Parser s a
commit p = P $ \ pSt _fl sc ->
                 -- We commit by prohibiting external sources from
                 -- overriding our failure function (by just ignoring
                 -- provided Failure values).
                 runP p pSt failure sc
{-# INLINE commit #-}

-- | A combination of 'fail' and 'commit': specify a failure that
--   cannot be recovered from.
failBad :: String -> Parser s a
failBad = commit . fail
{-# INLINE failBad #-}

-- -----------------------------------------------------------------------------
-- Combinators

-- | Return the next token from the input source.
next :: (ParseInput s) => Parser s (Token s)
next = satisfy (const True)
{-# INLINE next #-}

-- | This parser succeeds if we've reached the end of our input, and
--   fails otherwise.
endOfInput :: (ParseInput s) => Parser s ()
endOfInput = P $ \ pSt fl sc ->
                   if isEmpty (unI $ input pSt)
                      then sc pSt ()
                      else fl pSt "Expected end of input"
{-# INLINE endOfInput #-}

-- | Return the next token from our input if it satisfies the given
--   predicate.  Unlike 'satisfy', use the token (if one was
--   available) to provide a custom error message.
satisfyWith :: (ParseInput s) => (Token s -> String) -> (Token s -> Bool)
               -> Parser s (Token s)
satisfyWith toE f = do inp <- ensure 1
                       let !t = inputHead inp
                       if f t
                          then put (inputTail inp) *> pure t
                          else fail (toE t)
{-# INLINE satisfyWith #-}

-- | Return the next token from our input if it satisfies the given
--   predicate.
satisfy :: (ParseInput s) => (Token s -> Bool) -> Parser s (Token s)
satisfy = satisfyWith (const "Token did not satisfy predicate")
{-# INLINE satisfy #-}

-- | Return the result of the first parser in the list that succeeds.
oneOf :: (ParseInput s) => [Parser s a] -> Parser s a
oneOf = foldr (<|>) (fail "Failed to parse any of the possible choices")
{-# INLINE oneOf #-}

-- | Parse as many tokens that satisfy the predicate as possible.
--   This is a more efficient (and possibly fused) version of @'many'
--   ('satisfy' p)@.
manySatisfy :: (ParseInput s) => (Token s -> Bool) -> Parser s s
manySatisfy f = go
  where
    go = P $ \ pSt fl sc ->
      let (pre,suf) = breakWhen f (unI $ input pSt)
      in if (isEmpty suf && more pSt /= Complete)
            then runP (needMoreInput *> go) pSt fl sc
            else sc (pSt { input = I suf }) pre
{-# INLINE manySatisfy #-}

-- TODO: consider splitting and merging rather than continually
-- requesting input then re-breaking.

-- | Parse as many tokens that satisfy the predicate as possible, but
--   require at least one.  This is a more efficient (and possibly
--   fused) version of @'some' ('satisfy' p)@.
someSatisfy :: (ParseInput s) => (Token s -> Bool) -> Parser s s
someSatisfy f = do r <- manySatisfy f
                   if isEmpty r
                      then fail "someSatisfy: failed"
                      else return r
{-# INLINE someSatisfy #-}

-- | Push some tokens back onto the front of the input stream ready
--   for re-parsing.
--
--   A possible use would be for expanding macros: parse a macro,
--   expand it, and push the expanded version back onto the stream
--   ready to parse normally.
reparse :: (ParseInput s) => s -> Parser s ()
reparse s = P $ \ pSt _fl sc -> sc (pSt { input = I s <> input pSt }) ()
{-# INLINE reparse #-}

-- -----------------------------------------------------------------------------
-- Separating/discarding combinators

-- | Parse a list of items separated by discarded junk.
sepBy :: (ParseInput s) => Parser s a -> Parser s sep -> Parser s [a]
sepBy p sep = sepBy1 p sep <|> pure []
{-# INLINE sepBy #-}

-- | Parse a non-empty list of items separated by discarded junk.
sepBy1 :: (ParseInput s) => Parser s a -> Parser s sep -> Parser s [a]
sepBy1 p sep = addStackTrace "When looking for a non-empty sequence with separators:"
               $ liftA2 (:) p (many (sep *> p))
{-# INLINE sepBy1 #-}

-- | Parse a bracketed item, discarding the brackets.
--
--   Note that if the opening and item parsers succeed but the closing
--   parser fails, the entire combinator will fail and usage of
--   @'<|>'@ may lead to confusion as to why it failed.  As such,
--   you may wish to consider applying the 'commit' combinator onto
--   the close parser.
bracket :: (ParseInput s) => Parser s bra -> Parser s ket -> Parser s a -> Parser s a
bracket open close p = open' *> p <* close'
  where
    open'  = addStackTrace "Missing opening bracket:" open
    close' = addStackTrace "Missing closing bracket:" close
{-# INLINE bracket #-}

-- | Parse a (possibly empty) list of items, discarding the start, end
--   and separator items.
--
--   Note that 'commit' is applied to the end parser, otherwise it may
--   be difficult to track down errors.
bracketSep :: (ParseInput s) => Parser s bra -> Parser s sep -> Parser s ket
              -> Parser s a -> Parser s [a]
bracketSep open sep close p = addStackTrace "Bracketed list of separated items:"
                              $ bracket open (commit close) (sepBy p sep)
-- Original polyparse implementation differs.
{-# INLINE bracketSep #-}

-- | @manyFinally e z@ parses a possibly-empty sequence of @e@'s,
--   terminated by a @t@ (which is discarded).  As parsing failures
--   could be due to either of these parsers not succeeding, both
--   possible errors are raised.
manyFinally :: (ParseInput s) => Parser s a -> Parser s z -> Parser s [a]
manyFinally p t = addStackTrace "In a list of items with a terminator:"
                  ( many p
                    -- If t succeeds, then it will do so here.
                    -- Otherwise, better error reporting!
                    <* oneOf' [ ("sequence terminator",t *> pure ())
                              , ("item in a sequence", p *> pure ())
                              ])
{-# INLINE manyFinally #-}

-- | As with 'manyFinally', but handles the case where the terminator
--   parser overlapSt with the element parser.  As such, at each stage
--   it first tries the terminator parser before attempting to find an
--   element.
--
--   If there's no risk of overlap between the two parsers, you should
--   probably use 'manyFinally'.
manyFinally' :: (ParseInput s) => Parser s a -> Parser s z -> Parser s [a]
manyFinally' p t = addStackTrace "In a list of items with a terminator:" go
  where
    go =     (t *> pure [])
         <|> liftA2 (:) (p <|> errCheck) go

    -- This isn't the best way to go about it (the @pure undefined@ is
    -- a tad fugly, but should never be an issue) but it works.
    errCheck = oneOf' [ ("sequence terminator",t *> pure undefined)
                      , ("item in a sequence", p)
                      ]
{-# INLINE manyFinally' #-}

-- -----------------------------------------------------------------------------
-- Counting combinators

-- | Run the provided parser the specified number of times.
--
--   The behaviour is identical to that of 'replicateM' but with
--   better error messages.
exactly :: Int -> Parser s a -> Parser s [a]
exactly n p = mapM toP $ enumFromThenTo n (n-1) 1
  where
    toP = (`addStackTrace` p) . msg
    msg c = "Expecting precisely " ++ show c ++ " item(s)."
{-# INLINE exactly #-}

-- | @upto n p@ will return a list of no more than @n@ values.
upto :: (ParseInput s) => Int -> Parser s a -> Parser s [a]
upto n p = foldr go (pure []) $ replicate n p
  where
    -- There's probably a nicer way of writing this, but I can't think
    -- how... explicit recursion?

    -- Not taking argument as we _know_ it's `p'... We just used
    -- replicate to get the number right.
    go _ lst = liftA2 (:) p lst <|> pure []
{-# INLINE upto #-}

-- -----------------------------------------------------------------------------
-- Chaining

-- | @chainParsers pb pa@ will parse the input with @pa@ and then
--   parse the result of that parse with @pb@.  Control (and remaining
--   original input) is returned to the caller.
--
--   @pb@ is /not/ assumed to consume all of its input (and any
--   remaining input is discarded).  If this is required, use @pb '<*'
--   'endOfInput'@.  Similarly if @pa@ is meant to consume all input.
--
--   Note that @pa@ will run completely and obtain its entire output
--   before passing it on to @pb@ rather than letting @pb@ consume it
--   lazily.
chainParsers :: (ParseInput b) => Parser b c -> Parser a b -> Parser a c
chainParsers pb pa
   = P $ \ pStA fl sc ->
         runP pa pStA  fl $
           \ pStA' inpB ->
              -- We need to explicitly run the parser and use a case
              -- statement, as the types for the failure and success
              -- cases won't match if we just use runP.
              --
              -- pa will deal with getting more input, so we don't
              -- have to worry about that here, hence why it's safe to
              -- use runParser.
              case fst $ runParser pb' inpB of
                Right a         -> sc pStA' a
                Left (adjErr,e) -> let adjE' = parseLog pStA' . adjustError adjErr
                                   in fl (pStA' { parseLog = adjE' }) e
  where
    pb' = addStackTrace "Running chained parser:" pb
{-# INLINE chainParsers #-}

-- -----------------------------------------------------------------------------
-- Manipulating error messages

{- $stacktraces

For adding informative error messages to your parsers, whilst it is
possible to use the lower-level combinators listed below, it is highly
recommended that you just use the combinators listed here.

These provide a more convenient \"wrapper\" ability to add onto your
parsers, and also provide some basic pretty-printing to make it easier
to follow the trail of errors.

These combinators are already used in some of the combinators defined
in this library.

-}

-- | A convenient combinator to specify what the error message of a
--   combinator should be.
failMessage :: (ParseInput s) => String -> Parser s a -> Parser s a
failMessage e = (`onFail` fail e)
{-# INLINE failMessage #-}

addTraceMessage :: String -> Parser s ()
addTraceMessage m = addStackTrace m (pure ())

-- | A convenient function to produce (reasonably) pretty stack traces
--   for parsing failures.
addStackTrace :: String -> Parser s a -> Parser s a
addStackTrace msg = (`adjustErr` adjE)
  where
    adjE = (msg'++) . (('\n':stackTraceLine:'\n':stackTracePoint)++)

    -- Need to do this so that when the next error message is added,
    -- it will be indented properly.
    msg' = allButFirstLine ind msg

    ind = (stackTraceLine:) . indentLine lenStackTraceMarker
{-# INLINE addStackTrace #-}

-- | As with 'addStackTrace' but raise the severity of the error (same
--   relationship as between 'failBad' and 'fail').
addStackTraceBad :: String -> Parser s a -> Parser s a
addStackTraceBad msg = addStackTrace msg . commit
{-# INLINE addStackTraceBad #-}

-- | Apply the transformation function on any error messages that
--   might arise from the provided parser.
adjustErr :: Parser s a -> (String -> String) -> Parser s a
adjustErr p f = P $ \ pSt fl sc ->
                       runP p (pSt { parseLog = parseLog pSt . f }) fl sc
{-# INLINE adjustErr #-}

-- | As with 'adjustErr', but also raises the severity (same
--   relationship as between 'failBad' and 'fail').
adjustErrBad :: Parser s a -> (String -> String) -> Parser s a
adjustErrBad = adjustErr . commit
{-# INLINE adjustErrBad #-}

-- | As with 'oneOf', but each potential parser is tagged with a
--   \"name\" for error reporting.  The process is as follows:
--
--   * If a parser succeeds, return the result.
--
--   * If a severe error occurs (i.e. at some point 'commit' was
--     used), return that.  Note that if a parser with a severe error
--     is earlier in the list than a parser that would otherwise be
--     successful, then the severe error is still thrown.
--
--   * Otherwise, return all error messages.
oneOf' :: (ParseInput s) => [(String,Parser s a)] -> Parser s a
oneOf' = go id
  where
    go errs [] = fail $ "Failed to parse any of the possible choices:\n"
                        ++ indent 2 (concatMap showErr (errs []))
    -- Can't use <|> here as we want access to `e'.  Otherwise this is
    -- pretty much a duplicate of onFail.
    go errs ((nm,p):ps) = P $ \ pSt fl sc ->
      let go' e = go (errs . ((nm,e):)) ps
          -- When we fail (and the parser isn't committed), recurse
          -- and try the next parser whilst saving the error message.
          fl' pSt' e = mergeIncremental pSt pSt' $
            \ pSt'' -> runP (go' $ parseLog pSt' e) pSt'' fl sc

          sc' pSt' = sc (prependAdditional pSt pSt')
          -- Put back in the original additional input.
      in runP p (ignoreAdditional pSt { parseLog = noAdj }) fl' sc'
           -- Note: we only consider the AdjErr from the provided
           -- parser, not the global one.

    showErr (nm,e) = "* " ++ nm ++ ":\n" ++ indent 4 e ++ "\n"
