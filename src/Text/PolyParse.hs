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
       , resultToEither
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
       , (Cat.>>>)
       , (Cat.<<<)
       ) where

import Text.PolyParse.TextManipulation
import Text.PolyParse.Types

import           Control.Applicative
import qualified Control.Category    as Cat

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
--   That is, @commit p1 <|> p2@ is equivalent to just @p1@ (though
--   also preventing any other usage of '(<|>)' that might occur).
commit :: Parser s a -> Parser s a
commit p = P $ \ inp adjE _fl sc ->
                 -- We commit by prohibiting external sources from
                 -- overriding our failure function (by just ignoring
                 -- provided Failure values).
                 runP p inp adjE failure sc
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
next = P $ \ inp add mr adjE fl sc -> maybe (fl inp add mr adjE "Ran out of input")
                                     -- Note that when we fail, we use
                                     -- the /original/ input (even
                                     -- though it's empty).
                                     (uncurry $ flip sc)
                                     (uncons inp)
{-# INLINE next #-}

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
satisfy f = P $ \ inp adjE fl sc ->
                  runP next inp adjE fl $
                       \ inp' x ->
                          if f x
                             then sc inp' x
                             else fl inp adjE "Token did not satisfy predicate"
                                  -- Note: use /original/ input stream
                                  -- when we fail, not the one with
                                  -- the first token taken off!
{-# INLINE satisfy #-}

-- | Return the result of the first parser in the list that succeeds.
oneOf :: [Parser s a] -> Parser s a
oneOf = foldr (<|>) (fail "Failed to parse any of the possible choices")
{-# INLINE oneOf #-}

-- | Parse as many tokens that satisfy the predicate as possible.
--   This is a more efficient (and possibly fused) version of
--   @'many' (satisfy p)@.
manySatisfy :: (ParseInput s) => (Token s -> Bool) -> Parser s s
manySatisfy f = P $ \ inp _adjE _fl sc ->
                  let (pre,suf) = breakWhen f inp
                  in sc suf pre
{-# INLINE manySatisfy #-}

-- | Parse as many tokens that satisfy the predicate as possible, but
--   require at least one.  This is a more efficient (and possibly
--   fused) version of @'some' (satisfy p)@.
someSatisfy :: (ParseInput s) => (Token s -> Bool) -> Parser s s
someSatisfy f = P $ \ inp adjE fl sc ->
                    let (pre,suf) = breakWhen f inp
                    in if isEmpty pre
                          then fl inp adjE "someSatisfy: failed"
                          else sc suf pre
{-# INLINE someSatisfy #-}

-- | Push some tokens back onto the front of the input stream ready
--   for re-parsing.
--
--   A possible use would be for expanding macros: parse a macro,
--   expand it, and push the expanded version back onto the stream
--   ready to parse normally.
reparse :: (ParseInput s) => s -> Parser s ()
reparse s = P $ \ inp _adjE _fl sc -> sc (s <> inp) ()
{-# INLINE reparse #-}

-- -----------------------------------------------------------------------------
-- Separating/discarding combinators

-- | Parse a list of items separated by discarded junk.
sepBy :: Parser s a -> Parser s sep -> Parser s [a]
sepBy p sep = sepBy1 p sep <|> pure []
{-# INLINE sepBy #-}

-- | Parse a non-empty list of items separated by discarded junk.
sepBy1 :: Parser s a -> Parser s sep -> Parser s [a]
sepBy1 p sep = addStackTrace "When looking for a non-empty sequence with separators:"
               $ liftA2 (:) p (many (sep *> p))
{-# INLINE sepBy1 #-}

-- | Parse a bracketed item, discarding the brackets.
--
--   Note that if the opening and item parsers succeed but the closing
--   parser fails, the entire combinator will fail and usage of
--   @'(<|>)'@ may lead to confusion as to why it failed.  As such,
--   you may wish to consider applying the 'commit' combinator onto
--   the close parser.
bracket :: Parser s bra -> Parser s ket -> Parser s a -> Parser s a
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
bracketSep :: Parser s bra -> Parser s sep -> Parser s ket
              -> Parser s a -> Parser s [a]
bracketSep open sep close p = addStackTrace "Bracketed list of separated items:"
                              $ bracket open (commit close) (sepBy p sep)
-- Original polyparse implementation differs.
{-# INLINE bracketSep #-}

-- | @manyFinally e z@ parses a possibly-empty sequence of @e@'s,
--   terminated by a @t@ (which is discarded).  As parsing failures
--   could be due to either of these parsers not succeeding, both
--   possible errors are raised.
manyFinally :: Parser s a -> Parser s z -> Parser s [a]
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
manyFinally' :: Parser s a -> Parser s z -> Parser s [a]
manyFinally' p t = addStackTrace "In a list of items with a terminator:" go
  where
    go = oneOf [ t *> pure []
               , liftA2 (:) p go
               , oneOf' [ ("sequence terminator",t *> pure [])
                        , ("item in a sequence", p *> pure [])
                        ]
               ]
{-# INLINE manyFinally' #-}

-- -----------------------------------------------------------------------------
-- Counting combinators

-- | Run the provided parser the specified number of times.
exactly :: Int -> Parser s a -> Parser s [a]
exactly n p = mapM toP $ enumFromThenTo n (n-1) 1
  where
    -- Actually, @exactly == replicateM@, but this way we get nicer
    -- error messages.
    toP = (`addStackTrace` p) . msg
    msg c = "Expecting precisely " ++ show c ++ " item(s)."
{-# INLINE exactly #-}

-- | @upto n p@ will return a list of no more than @n@ values.
upto :: Int -> Parser s a -> Parser s [a]
upto n p = foldr go (pure []) $ replicate n p
  where
    -- There's probably a nicer way of writing this, but I can't think
    -- how... explicit recursion?

    -- Not taking argument as we _know_ it's `p'... We just used
    -- replicate to get the number right.
    go _ lst = liftA2 (:) p lst <|> pure []
{-# INLINE upto #-}

-- -----------------------------------------------------------------------------
-- Manipulating error messages

{- $stacktraces

For adding informative error messages to your parsers, whilst it is
possible to just use the lower-level combinators listed below, it is
highly recommended that you just use the combinators listed here.

These provide a more convenient \"wrapper\" ability to add onto your
parsers, and also provide some basic pretty-printing to make it easier
to follow the trail of errors.

These combinators are already used in some of the combinators defined
in this library.

-}

-- | A convenient combinator to specify what the error message of a
--   combinator should be.
failMessage :: String -> Parser s a -> Parser s a
failMessage e = (`onFail` fail e)
{-# INLINE failMessage #-}

-- | A convenient function to produce (reasonably) pretty stack traces
--   for parsing failures.
addStackTrace :: String -> Parser s a -> Parser s a
addStackTrace msg = (`adjustErr`((msg'++) . (('\n':stackTracePoint)++)))
  where
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
adjustErr p f = P $ \ inp adjE fl sc ->
                       runP p inp (adjE . f) fl sc
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
oneOf' :: [(String,Parser s a)] -> Parser s a
oneOf' = go id
  where
    go errs [] = fail $ "Failed to parse any of the possible choices:\n"
                        ++ indent 2 (concatMap showErr (errs []))
    -- Can't use <|> here as we want access to `e'.  Otherwise this is
    -- pretty much a duplicate of onFail.
    go errs ((nm,p):ps) = P $ \ inp adjE fl sc ->
      let go' e = go (errs . ((nm,e):)) ps
          -- When we fail (and the parser isn't committed), recurse
          -- and try the next parser whilst saving the error message.
          fl' _inp adjE' e = runP (go' $ adjE' e) inp adjE fl sc
      in runP p inp noAdj fl' sc
         -- Note: we only consider the AdjErr from the provided
         -- parser, not the global one.

    showErr (nm,e) = "* " ++ nm ++ ":\n" ++ indent 4 e
