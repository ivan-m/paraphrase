{-# LANGUAGE BangPatterns, FlexibleContexts #-}
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
       , resultToEither
       , EitherResult
         -- ** Parser input
       , TokenStream (Stream, Token)
       , ParseInput (..)
       , AsChar8(..)
       , Word8Input(..)

         -- * Parser combinators
       , next
       , token
       , satisfy
       , satisfyWith
       , endOfInput
       , oneOf
       , bracket
       , reparse
         -- ** Commitment
       , commit
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
       , ParseError (..)
       , BracketType (..)
       , TaggedError (..)
       , PrettyValue (..)
       , ParsingErrors
       , failWith
       , failBadWith
       , finalError
       , completeLog
       , prettyLog
       , prettyDetailedLog
         -- ** Convenience functions
         -- $stacktraces
       , (<?>)
       , failMessage
       , addStackTrace
       , addStackTraceBad
       , addErrOnFailure
         -- ** Low-level error adjustment
       , oneOf'

         -- * Re-exported for extra combinators
       , module Control.Applicative
       ) where

import Text.Paraphrase.Additional
import Text.Paraphrase.Errors
import Text.Paraphrase.Inputs
import Text.Paraphrase.Types
import Text.Paraphrase.Wrappers

import Control.Applicative
import Data.IsNull         (isNull)

-- -----------------------------------------------------------------------------
-- Commitment

-- | Prevent any backtracking from taking place by emphasising that
--   any failures from this parser are more severe than usual.
--
--   That is, @commit p1 \<|\> p2@ is equivalent to just @p1@ (though
--   also preventing any other usage of @'<|>'@ that might occur).
commit :: (ParseInput s) => Parser e s a -> Parser e s a
commit = addStackTrace Committed . commitNoLog
{-# INLINE commit #-}

-- | A combination of 'failWith' and 'commit': specify a failure that
--   cannot be recovered from.
failBadWith :: (ParseInput s) => ParseError e s -> Parser e s a
failBadWith = commit . failWith
{-# INLINE failBadWith #-}

-- -----------------------------------------------------------------------------
-- Combinators

-- | Return the next token from the input source.
next :: (ParseInput s) => Parser e s (Token s)
next = satisfy (const True)
{- INLINE next #-}

-- | This parser succeeds if we've reached the end of our input, and
--   fails otherwise.
endOfInput :: (ParseInput s) => Parser e s ()
endOfInput = P $ \ pSt fl sc ->
                   if isEmpty (input pSt)
                      then sc pSt ()
                      else fl pSt ExpectedEndOfInput
{-# INLINE endOfInput #-}

-- | Return the next token from our input if it satisfies the given
--   predicate.  Unlike 'satisfy', use the token (if one was
--   available) to provide a custom error message.
satisfyWith :: (ParseInput s) => (Token s -> ParseError e s) -> (Token s -> Bool)
               -> Parser e s (Token s)
satisfyWith toE f = do
  inp <- getAtLeast 1
  let !t = inputHead inp
  if f t
     then put (inputTail inp) *> pure t
     else failWith (toE t)
{-# INLINE satisfyWith #-}

-- | Return the next token from our input if it satisfies the given
--   predicate.
satisfy :: (ParseInput s) => (Token s -> Bool) -> Parser e s (Token s)
satisfy = satisfyWith UnexpectedToken
{-# INLINE satisfy #-}

token :: (ParseInput s, Eq (Token s)) => Token s -> Parser e s (Token s)
token t = satisfyWith (ExpectedButFound t) (t==)
{-# INLINE token #-}

-- | Return the result of the first parser in the list that succeeds.
oneOf :: (ParseInput s) => [Parser e s a] -> Parser e s a
oneOf = wrapCommitment . foldr onFail (failWith NoParserSatisfied)
{-# INLINE oneOf #-}
-- Safe to wrap commitment only on the outside: if /one/ of them ends
-- up being committed, then we shouldn't be able to backtrack to the
-- next one anyway.  We only care about outside commitment.

-- | Parse as many tokens that satisfy the predicate as possible.
--   This is a more efficient (and possibly fused) version of @'many'
--   ('satisfy' p)@.
manySatisfy :: (ParseInput s) => (Token s -> Bool) -> Parser e s (Stream s)
manySatisfy f = go
  where
    go = P $ \ pSt fl sc ->
      let (pre,suf) = breakWhen f (input pSt)
      in if (isEmpty suf && more pSt /= Complete)
            then runP (needMoreInput *> go) pSt fl sc
            else sc (pSt { input = suf }) pre
{-# INLINE manySatisfy #-}

-- TODO: consider splitting and merging rather than continually
-- requesting input then re-breaking.

-- | Parse as many tokens that satisfy the predicate as possible, but
--   require at least one.  This is a more efficient (and possibly
--   fused) version of @'some' ('satisfy' p)@.
someSatisfy :: (ParseInput s) => (Token s -> Bool) -> Parser e s (Stream s)
someSatisfy f = addStackTrace PredicateNotSatisfied
                ( do r <- manySatisfy f
                     if isNull r
                        then next >>= failWith . UnexpectedToken
                             -- This will also take care of empty input
                             -- case.
                        else pure r
                ) <?> "someSatisfy"
{-# INLINE someSatisfy #-}

-- | Push some tokens back onto the front of the input stream ready
--   for re-parsing.
--
--   A possible use would be for expanding macros: parse a macro,
--   expand it, and push the expanded version back onto the stream
--   ready to parse normally.
reparse :: (ParseInput s) => Stream s -> Parser e s ()
reparse s = addStackTrace (Reparse s)
              (P $ \ pSt _fl sc -> sc (pSt { input = s `prependStream` input pSt }) ())
{-# INLINE reparse #-}

-- -----------------------------------------------------------------------------
-- Separating/discarding combinators

-- | Parse a list of items separated by discarded junk.
sepBy :: (ParseInput s) => Parser e s a -> Parser e s sep -> Parser e s [a]
sepBy p sep = sepBy1 p sep <|> pure []
{-# INLINE sepBy #-}

-- | Parse a non-empty list of items separated by discarded junk.
sepBy1 :: (ParseInput s) => Parser e s a -> Parser e s sep -> Parser e s [a]
sepBy1 p sep = addStackTrace PredicateNotSatisfied
               (liftA2 (:) p (many (sep *> p))) <?> "sepBy1"
{-# INLINE sepBy1 #-}

-- | Parse a bracketed item, discarding the brackets.
--
--   Note that if the opening and item parsers succeed but the closing
--   parser fails, the entire combinator will fail and usage of
--   @'<|>'@ may lead to confusion as to why it failed.  As such,
--   you may wish to consider applying the 'commit' combinator onto
--   the close parser.
bracket :: (ParseInput s) => Parser e s bra -> Parser e s ket -> Parser e s a -> Parser e s a
bracket open close p = open' *> p <* close'
  where
    open'  = addErrOnFailure (MissingBracket OpenBracket)  open
    close' = addErrOnFailure (MissingBracket CloseBracket) close
{-# INLINE bracket #-}

-- | Parse a (possibly empty) list of items, discarding the start, end
--   and separator items.
--
--   Note that 'commit' is applied to the end parser, otherwise it may
--   be difficult to track down errors.
bracketSep :: (ParseInput s) => Parser e s bra -> Parser e s sep -> Parser e s ket
              -> Parser e s a -> Parser e s [a]
bracketSep open sep close p =
  ((addStackTrace (MissingBracket OpenBracket) open) *>
    ( (close' *> pure [])
      <|>
      (liftA2 (:) p (manyFinally (sep *> p) (commit close')))
    )
  ) <?> "bracketSep"
  where
    close' = addErrOnFailure (MissingBracket CloseBracket) close
{-# INLINE bracketSep #-}

-- | @manyFinally e z@ parses a possibly-empty sequence of @e@'s,
--   terminated by a @t@ (which is discarded).  As parsing failures
--   could be due to either of these parsers not succeeding, both
--   possible errors are raised.
manyFinally :: (ParseInput s) => Parser e s a -> Parser e s z -> Parser e s [a]
manyFinally p t = addStackTrace ListWithTerminator
                  ( many p
                    -- If t succeeds, then it will do so here.
                    -- Otherwise, better error reporting!
                    <* oneOf' [ ("item in a sequence", p *> pure ())
                              , ("sequence terminator",t *> pure ())
                              ])
{-# INLINE manyFinally #-}

-- | As with 'manyFinally', but handles the case where the terminator
--   parser overlaps with the element parser.  As such, at each stage
--   it first tries the terminator parser before attempting to find an
--   element.
--
--   If there's no risk of overlap between the two parsers, you should
--   probably use 'manyFinally'.
manyFinally' :: (ParseInput s) => Parser e s a -> Parser e s z -> Parser e s [a]
manyFinally' p t = addStackTrace ListWithTerminator go
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
exactly :: (ParseInput s) => Int -> Parser e s a -> Parser e s [a]
exactly n p = go n
  where
    go !c
      | c <= 0    = pure []
      | otherwise = (:) <$> MissingItemCount c `addStackTrace` p <*> go (c-1)
      -- Need to replace that mempty...
{-# INLINE exactly #-}

-- | @upto n p@ will return a list of no more than @n@ values.
upto :: (ParseInput s) => Int -> Parser e s a -> Parser e s [a]
upto n p = go n
  where
    go !c
      | c <= 0    = pure []
      | otherwise = ((:) <$> p <*> go (c-1)) <|> pure []
{-# INLINE upto #-}

-- -----------------------------------------------------------------------------
-- Chaining

-- | @chainParser es pb pa@ will parse the input with @pa@ and then
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
chainParsers :: (ParseInput b) => (e -> e') -> Parser e b c
                -> Parser e' a b -> Parser e' a c
chainParsers f pb pa
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
              case changeErrorE f . fst $ runParser pb' (getStream inpB) of
                Right a  -> sc pStA' a
                Left plb -> fl pStA' (SubLog . streamToDoc . completeLog $ plb)
  where
    pb' = addStackTrace ChainedParser pb
{-# INLINE chainParsers #-}

-- -----------------------------------------------------------------------------
-- Manipulating error messages

{- $stacktraces

It is highly recommended that you take advantage of these functions to
add more informative error messages to your parsers.

With the use of 'prettyLog' and 'prettyDetailedLog' a pretty-printed
stack trace of error messages will be printed to help you determine
what went wrong.

Alternatively, by use of 'completeLog' you can obtain the entire
type-ful list of error messages (tagged with the current input at that
point in time) and be able to navigate through them in ghci to help
you determine where the actual error occurred.

These functions are already used in most of the combinators defined in
this library.

Note that functions such as 'failMessage' will /not/ work when applied
to a 'commit'ted combinator, as they rely upon backtracking (which
commit prohibits).

-}

-- | A convenient combinator to specify what the error message of a
--   combinator should be.
failMessage :: (ParseInput s) => ParseError e s -> Parser e s a -> Parser e s a
failMessage e = (`onFail` failWith e)
{-# INLINE failMessage #-}

-- | As with 'addStackTrace' but raise the severity of the error (same
--   relationship as between 'failBadWith' and 'failWith').
addStackTraceBad :: (ParseInput s) => ParseError e s
                    -> Parser e s a -> Parser e s a
addStackTraceBad e = addStackTrace e . commit
{-# INLINE addStackTraceBad #-}

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
oneOf' :: (ParseInput s) => [(String,Parser e s a)] -> Parser e s a
oneOf' = wrapCommitment . go id
  where
    go errs [] = failWith (NamedSubLogs (errs []))

    -- Can't use <|> here as we want access to `e'.  Otherwise this is
    -- pretty much a duplicate of onFail.
    go errs ((nm,p):ps) =
      let go' e = go (errs . ((nm,e):)) ps
      in onFailWith go' p
