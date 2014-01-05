{-# LANGUAGE RankNTypes, TypeFamilies #-}
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
       , eof
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

import           Control.Applicative
import qualified Control.Category    as Cat
import           Control.Monad       (MonadPlus (..))
import           Data.Monoid

-- Sources
{-
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as LT
-}

-- -----------------------------------------------------------------------------

{-

Coding conventions.

* Use variables like @p@ for parsers (exceptions are for combinators
  like 'bracket', where the names for the discarded parsers tend to be
  more descriptive in nature.

* As such, don't follow the usual convention of using @p@ be used for
  predicates; just use @f@ instead.

See also the provided conventions for referring to values of type
'ErrMsg', 'AdjustErr', 'Failure' and 'Success'.

 -}

-- -----------------------------------------------------------------------------

-- | The types which we know how to manipulate at a low-level.  This
--   class defines the minimum that is required to use all parser
--   combinators.
--
--   Please note that not /all/ combinators require the input source
--   to be an instance of this class; this is to keep the types as
--   generic as possible.
--
--   Unless you're defining a new input source, you probably do not
--   need to examine the methods of this class.
class (Monoid s) => ParseInput s where

  -- | The elements of this
  type Token s

  -- | Attempt to extract the first token.
  uncons :: s -> Maybe (Token s, s)

  -- | Is the input empty?
  isEmpty :: s -> Bool

  -- | Split the stream where the predicate is no longer satisfied
  --   (that is, the @fst@ component contains the largest possible
  --   prefix where all values satisfy the predicate, and the @snd@
  --   component contains the latter).
  breakWhen :: (Token s -> Bool) -> s -> (s,s)

instance ParseInput [a] where
  type Token [a] = a

  uncons []     = Nothing
  uncons (a:as) = Just (a,as)

  isEmpty = null

  breakWhen = span

-- -----------------------------------------------------------------------------
-- Result

-- | The possible results from running a parser on some input,
--   parametrised on the input source @s@.  The remaining input is
--   also returned.
data Result s a = Success s a
                | Failure s String
                deriving (Eq, Ord, Show, Read)

instance Functor (Result s) where
  fmap f (Success s a) = Success s (f a)
  fmap _ (Failure s e) = Failure s e

-- | Convert the result into an 'Either' value.
resultToEither :: Result s a -> (Either String a, s)
resultToEither (Success s a) = (Right a, s)
resultToEither (Failure s e) = (Left e, s)

-- -----------------------------------------------------------------------------
-- Parser definition

-- | The overall parser type, parametrised over the input type @s@.
--
--   The various combinators back-track by default (use 'commit' if
--   you wish to prevent back-tracking).
--
--   Use the (otherwise-fraught-with-danger) 'fail' method of the
--   'Monad' instance to report a parsing failure; alternatively use
--   the various error-reporting combinators listed below.
newtype Parser s a = P {
  -- Our parser is actually a function of functions!
  runP :: forall r.
          s             -- The input.
       -> AdjErr        -- What to do with any error messages.
       -> Failure s   r -- What to do when we fail.
       -> Success s a r -- What to do when we succeed.
       -> Result  s   r
  }

-- An alias for internal purposes to signify what a 'String' input
-- means.
--
-- CONVENTION: a value of this type is called something like @e@.
type ErrMsg = String

-- How to /Adj/ust an /Err/or message.  This is an implicit difference
-- list.  This is used to be able to add additional messages to an
-- error failure such as a stack trace of all combinators being used.
--
-- CONVENTION: a value of this type is called something like @adjE@.
type AdjErr = ErrMsg -> ErrMsg

-- What to do when we fail a parse; @s@ is the input type.
--
-- CONVENTION: a value of this type is called something like @fl@.
type Failure s   r = s -> AdjErr -> ErrMsg -> Result s r

-- What to do when a parse is successful; @s@ is the input type, @a@
-- is the result of the parse, @r@ is the output type.
--
-- CONVENTION: a value of this type is called something like @sc@.
type Success s a r = s           -> a      -> Result s r

-- Start the difference list by doing nothing.
noAdj :: AdjErr
noAdj = id

-- Dum... Dum... Dum... DUMMMMMM!!!  The parsing has gone all wrong,
-- so apply the error-message adjustment and stop doing anything.
failure :: Failure s r
failure s adjE e = Failure s (adjE $ indMsg e)
  where
    indMsg = allButFirstLine (indent lenStackTracePoint)

-- Hooray!  We're all done here, and a job well done!
successful :: Success s a a
successful = Success

-- | Run the parser on the provided input, providing the raw 'Result'
--   value.
parseInput :: Parser s a -> s -> Result s a
parseInput p inp = runP p inp noAdj failure successful

-- | Run a parser.
runParser :: Parser s a -> s -> (Either String a, s)
runParser = (resultToEither .) . parseInput

-- | Run a parser, assuming it succeeds.  If the parser fails, use
--   'error' to display the message.
runParser' :: Parser s a -> s -> a
runParser' p inp = case fst $ runParser p inp of
                     Right a  -> a
                     Left err -> error ('\n':err)

-- -----------------------------------------------------------------------------
-- Instances

instance Functor (Parser s) where
  fmap = fmapP
  {-# INLINE fmap #-}

fmapP :: (a -> b) -> Parser s a -> Parser s b
fmapP f pa = P $ \ inp adjE fl sc ->
                 runP pa inp adjE fl $ \ inp' a ->
                                          sc inp' (f a)
{-# INLINE fmapP #-}

instance Applicative (Parser s) where
  pure = returnP
  {-# INLINE pure #-}

  (<*>) = apP
  {-# INLINE (<*>) #-}

  (*>) = ignFirstP
  {-# INLINE (*>) #-}

  (<*) = discard
  {-# INLINE (<*) #-}

instance Alternative (Parser s) where
  empty = failP "empty"
  {-# INLINE empty #-}

  (<|>) = onFail
  {-# INLINE (<|>) #-}

instance Monad (Parser s) where
  return = returnP
  {-# INLINE return #-}

  (>>=) = bindP
  {-# INLINE (>>=) #-}

  (>>) = ignFirstP
  {-# INLINE (>>) #-}

  fail = failP
  {-# INLINE fail #-}

instance MonadPlus (Parser s) where
  mzero = failP "mzero"
  {-# INLINE mzero #-}

  mplus = onFail
  {-# INLINE mplus #-}

-- | No further input should be consumed from the @id@ definition (as
--   it returns all input).
instance Cat.Category Parser where
  id = allInputP
  {-# INLINE id #-}

  (.) = chainParsers
  {-# INLINE (.) #-}

-- ALL THE INPUTZ!!!
allInputP :: Parser s s
allInputP = P $ \ inp _adjE _fl sc -> sc (error "No more input to consume.") inp
{-# INLINE allInputP #-}

-- | @chainParsers pt ps@ will parse the input with @ps@ and then
--   parse the result of that parse with @pt@.  Control (and remaining
--   original input) is returned to the caller.
--
--   @pt@ is /not/ assumed to consume all of its input (and any
--   remaining input is discarded).  If this is required, use @pt <*
--   'eof'@.  Similarly if @ps@ is meant to consume all input.
--
--   This function is identical to @'(Cat..)'@ and @'(Cat.<<<)'@ from
--   "Control.Category".
chainParsers :: Parser t a -> Parser s t -> Parser s a
chainParsers pt ps = P $ \ inpS adjE fl sc ->
                           runP ps inpS adjE fl $ \ inpS' inpT ->
                                case parseInput pt inpT of
                                  Success _ a -> sc inpS' a
                                  Failure _ e -> fl inpS' adjE e
{-# INLINE chainParsers #-}


returnP :: a -> Parser s a
returnP a = P $ \ inp _adjE _fl sc -> sc inp a
{-# INLINE returnP #-}

-- Explicit version of @pa >>= const pb@.
ignFirstP :: Parser s a -> Parser s b -> Parser s b
ignFirstP pa pb = P $ \ inp adjE fl sc ->
                        runP pa inp adjE fl $ \ inp' _a ->
                                                runP pb inp' adjE fl sc
{-# INLINE ignFirstP #-}

discard :: Parser s a -> Parser s b -> Parser s a
discard pa pb = P $ \ inp adjE fl sc ->
                  let sc' a inp' _b = sc inp' a
                      -- Ignore the provided result and use the one
                      -- you obtained earlier.
                  in runP pa inp adjE fl $ \ inp' a ->
                                              runP pb inp' adjE fl (sc' a)
{-# INLINE discard #-}

apP :: Parser s (a -> b) -> Parser s a -> Parser s b
apP pf pa = P $ \ inp adjE fl sc ->
                  runP pf inp adjE fl
                       $ \ inp' f -> runP pa inp' adjE fl
                                          $ \ inp'' a -> sc inp'' (f a)
{-# INLINE apP #-}

failP :: String -> Parser s a
failP e = P $ \ inp adjE fl _sc -> fl inp adjE e
{-# INLINE failP #-}

bindP ::  Parser s a -> (a -> Parser s b) -> Parser s b
bindP p f = P $ \ inp adjE fl sc -> runP p inp adjE fl $
                 -- Get the new parser and run it.
                  \inp' a -> runP (f a) inp' adjE fl sc
{-# INLINE bindP #-}

onFail :: Parser s a -> Parser s a -> Parser s a
onFail p1 p2 = P $ \ inp adjE fl sc ->
               let fl' _inp' _adjE' _e = runP p2 inp adjE fl sc
                   -- If we fail, run parser p2 instead.  Don't use
                   -- the provided @AdjErr@ value, get the "global"
                   -- one instead (as we don't want p1's stack
                   -- traces).

                   sc' inp'           = sc inp'
                   -- Re-defined in case we need to change it for
                   -- streaming.
               in runP p1 inp adjE fl' sc'
{-# INLINE onFail #-}

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
next = P $ \ inp adjE fl sc -> maybe (fl inp adjE "Ran out of input (EOF)")
                                     -- Note that when we fail, we use
                                     -- the /original/ input (even
                                     -- though it's empty).
                                     (uncurry $ flip sc)
                                     (uncons inp)
{-# INLINE next #-}

-- | This parser succeeds if we've reached the end of our input, and
--   fails otherwise.  \"eof\" is short for "end of file", and the
--   term is used primarily for historical reasons.
eof :: (ParseInput s) => Parser s ()
eof = P $ \ inp adjE fl sc ->
            if isEmpty inp
               then sc inp ()
               else fl inp adjE "Expected end of input (EOF)"
{-# INLINE eof #-}

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
                  ((many p <* t)
                   <|> oneOf' [ ("sequence terminator",t *> pure [])
                              , ("item in a sequence", p *> pure [])
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

stackTraceMarker :: String
stackTraceMarker = "-> "

lenStackTraceMarker :: Int
lenStackTraceMarker = length stackTraceMarker

stackTraceLine :: Char
stackTraceLine = '|'

stackTracePoint :: String
stackTracePoint = stackTraceLine : stackTraceMarker

lenStackTracePoint :: Int
lenStackTracePoint = length stackTracePoint

-- | Map a function over all but the first line of text.  Useful when
--   you want all subsequent lines indented.
allButFirstLine :: (String -> String) -> String -> String
allButFirstLine f msg = case lines msg of
                          [_]      -> msg
                          (ln:lns) -> unlines $ ln : map f lns
                          _        -> msg
{-# INLINE allButFirstLine #-}

-- | A convenience function for use with 'adjustErr' useful for
--   formatting error messages; indents /all/ lines by a fixed amount.
indent :: Int -> String -> String
indent n = unlines . map (indentLine n) . lines
{-# INLINE indent #-}

-- | As with 'indent' but assumes the error message is a single line.
indentLine :: Int -> String -> String
indentLine n = (replicate n ' ' ++)
{-# INLINE indentLine #-}

-- | As with 'oneOf', but each potential parser is tagged with a
--   \"name\" for error reporting.  The process is as follows:
--
--   * If a parser succeeds, return the result.
--
--   * If a severe error occurs (i.e. at some point @commit@ was
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
    -- Can't use <|> here as we want access to `e'.
    go errs ((nm,p):ps) = P $ \ inp adjE fl sc ->
      let go' e = go (errs . ((nm,e):)) ps
          -- When we fail (and the parser isn't committed), recurse
          -- and try the next parser whilst saving the error message.
          fl' _inp _adjE e = runP (go' e) inp adjE fl sc
      in runP p inp adjE fl' sc

    showErr (nm,e) = "* " ++ nm ++ ":\n" ++ indent 4 e
