{- |
   Module      : Text.PolyParse
   Description : Experimental polyparse reimplementation
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is an experiment in creating a modern combinator-parsing
   library based upon the "Text.ParserCombinators.Poly.Base",
   etc. modules found in the
   <http://hackage.haskell.org/package/polyparse polyparse> library.

 -}
module Text.PolyParse
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

         -- * Parser combinators
       , next
       , satisfy
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

import Text.PolyParse.TextManipulation
import Text.PolyParse.Types

import Control.Applicative
import Data.Monoid

-- Sources
{-
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
-}

-- -----------------------------------------------------------------------------
-- Commitment

-- | Prevent any backtracking from taking place by emphasising that
--   any failures from this parser are more severe than usual.
--
--   That is, @commit p1 \<|\> p2@ is equivalent to just @p1@ (though
--   also preventing any other usage of @'<|>'@ that might occur).
commit :: Parser s a -> Parser s a
commit p = P $ \ inp add mr adjE _fl sc ->
                 -- We commit by prohibiting external sources from
                 -- overriding our failure function (by just ignoring
                 -- provided Failure values).
                 runP p inp add mr adjE failure sc
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
next = needAtLeast 1
       *> P (\ inp add mr adjE fl sc -> maybe (fl inp add mr adjE "Ran out of input")
                                        -- Note that when we fail, we
                                        -- use the /original/ input
                                        -- (even though it's empty).
                                        (\ (t,inp') -> sc (I inp') add mr t)
                                        (uncons (unI inp)))
{-# INLINE next #-}

-- TODO: test if we actually need uncons to return a Maybe now...

-- | This parser succeeds if we've reached the end of our input, and
--   fails otherwise.
endOfInput :: (ParseInput s) => Parser s ()
endOfInput = P $ \ inp add mr adjE fl sc ->
                   if isEmpty (unI inp)
                      then sc inp add mr ()
                      else fl inp add mr adjE "Expected end of input"
{-# INLINE endOfInput #-}

-- | Return the next token from our input if it satisfies the given
--   predicate.
satisfy :: (ParseInput s) => (Token s -> Bool) -> Parser s (Token s)
satisfy f = P $ \ inp add mr adjE fl sc ->
                  runP next inp add mr adjE fl $
                       \ inp' add' mr' x ->
                          if f x
                             then sc inp' add' mr' x
                             else fl inp add mr adjE "Token did not satisfy predicate"
                                  -- Note: use /original/ input stream
                                  -- when we fail, not the one with
                                  -- the first token taken off!
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
    go = P $ \ inp add mr adjE fl sc ->
      let (pre,suf) = breakWhen f (unI inp)
      in if (isEmpty suf && mr /= Complete)
            then runP (needMoreInput *> go) inp add mr adjE fl sc
            else sc (I suf) add mr pre
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
reparse s = P $ \ inp add mr _adjE _fl sc -> sc (I s <> inp) add mr ()
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
--   parser overlaps with the element parser.  As such, at each stage
--   it first tries the terminator parser before attempting to find an
--   element.
--
--   If there's no risk of overlap between the two parsers, you should
--   probably use 'manyFinally'.
--
--   Note that this combinator raises a severe (i.e. it uses 'commit')
--   error if the element parser succeeds at least once but then
--   neither the element parser nor the termination parser succeed
--   after that.
manyFinally' :: (ParseInput s) => Parser s a -> Parser s z -> Parser s [a]
manyFinally' p t = addStackTrace "In a list of items with a terminator:" go
  where
    go =     (t *> pure [])
         <|> liftA2 (:) p (commit go)
         <|> oneOf' [ ("sequence terminator",t *> pure [])
                    , ("item in a sequence", p *> pure [])
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

-- | @chainParsers pt ps@ will parse the input with @ps@ and then
--   parse the result of that parse with @pt@.  Control (and remaining
--   original input) is returned to the caller.
--
--   @pt@ is /not/ assumed to consume all of its input (and any
--   remaining input is discarded).  If this is required, use @pt '<*'
--   'endOfInput'@.  Similarly if @ps@ is meant to consume all input.
--
--   Note that @ps@ will run completely and obtain its entire output
--   before passing it on to @pt@ rather than letting @pt@ consume it
--   lazily.
chainParsers :: (ParseInput t) => Parser t a -> Parser s t -> Parser s a
chainParsers pt ps
   = P $ \ inpS addS mrS adjE fl sc ->
         runP ps inpS addS mrS adjE fl $
           \ inpS' addS' mrS' inpT ->
              -- We need to explicitly run the parser and use a case
              -- statement, as the types for the failure and success
              -- cases won't match if we just use runP.
              --
              -- ps will deal with getting more input, so we don't
              -- have to worry about that here, hence why it's safe to
              -- use runParser.
              case fst $ runParser pt' inpT of
                Right a         -> sc inpS' addS' mrS' a
                Left (adjErr,e) -> let adjE' = adjE . adjustError adjErr
                                   in fl inpS' addS' mrS' adjE' e
  where
    pt' = addStackTrace "Running chained parser:" pt
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
addStackTraceBad msg = addStackTraceBad msg . commit
{-# INLINE addStackTraceBad #-}

-- | Apply the transformation function on any error messages that
--   might arise from the provided parser.
adjustErr :: Parser s a -> (String -> String) -> Parser s a
adjustErr p f = P $ \ inp add mr adjE fl sc ->
                       runP p inp add mr (adjE . f) fl sc
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
    go errs ((nm,p):ps) = P $ \ inp add mr adjE fl sc ->
      let go' e = go (errs . ((nm,e):)) ps
          -- When we fail (and the parser isn't committed), recurse
          -- and try the next parser whilst saving the error message.
          fl' inp' add' mr' adjE' e = mergeIncremental inp add mr inp' add' mr' $
            \ inp'' add'' mr'' -> runP (go' $ adjE' e) inp'' add'' mr'' adjE fl sc
      in ignoreAdditional inp add mr $
           -- Note: we only consider the AdjErr from the provided
           -- parser, not the global one.
           \ inp' add' mr' -> runP p inp' add' mr' noAdj fl' sc

    showErr (nm,e) = "* " ++ nm ++ ":\n" ++ indent 4 e ++ "\n"
