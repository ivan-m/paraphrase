{-# LANGUAGE BangPatterns, FlexibleContexts, GeneralizedNewtypeDeriving,
             OverloadedStrings, RankNTypes, StandaloneDeriving, TypeFamilies,
             UndecidableInstances #-}
{- |
   Module      : Text.Paraphrase.Types
   Description : Definition of types
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Definition of types, classes and low-level functions.

 -}
module Text.Paraphrase.Types where

import Text.Paraphrase.Errors
import Text.Paraphrase.Inputs (ParseInput (..), TokenStream (..))

import Control.Applicative
import Control.DeepSeq     (NFData (rnf))
import Control.Monad       (MonadPlus (..))
import Data.Monoid

-- -----------------------------------------------------------------------------

{-

Coding conventions.

* Use variables like @p@ for parsers (exceptions are for combinators
  like 'bracket', where the names for the discarded parsers tend to be
  more descriptive in nature.

* As such, don't follow the usual convention of using @p@ be used for
  predicates; just use @f@ instead.

See also the provided conventions for referring to values of type
'Input', 'Additional', 'More', 'ErrMsg', 'AdjErr', 'Failure' and
'Success'.

 -}

-- -----------------------------------------------------------------------------
-- Result

-- | The possible results from running a parser on some input,
--   parametrised on the input source @s@.  The remaining input is
--   also returned.
--
--   The @Failure@ case contains the bare error message as well as the
--   function that will produce any additional error messages (stack
--   traces, etc.) if so desired.
data Result s a = Success s a
                | Failure s (ParsingErrors s)
                | Partial   (ParsingErrors s) (Stream s -> Result s a)
                  -- ^ Indicates that the parser requires more input
                  --   to continue.
                  --
                  --   The 'ParseError' is for use with
                  --   'resultToEither' and can be safely ignored in
                  --   your own code.

-- | Nonsensical for 'Partial'.
instance (ParseInput s, Show s, Show (Stream s), Show (Token s), Show a) => Show (Result s a) where
  showsPrec d r = showParen (d > 10) $
                    case r of
                      Success s a   -> showString "Success "
                                       . shows s
                                       . showString " "
                                       . shows a
                      Failure s pl  -> showString "Failure "
                                       . shows s
                                       . showString " "
                                       . showsPrec 11 pl
                      Partial pl _p -> showString "Partial "
                                       . showsPrec 11 pl
                                       . showString " "
                                       . showString "\"<continuation>\""

instance Functor (Result s) where
  fmap f (Success s a)    = Success s (f a)
  fmap _ (Failure s pl)   = Failure s pl
  fmap f (Partial pl cnt) = Partial pl (fmap f . cnt)

instance (ParseInput s, NFData s, NFData (Stream s), NFData (Token s), NFData a) => NFData (Result s a) where
  rnf (Success s a)  = rnf s `seq` rnf a
  rnf (Failure s pl) = rnf s `seq` rnf pl
  rnf (Partial pl _) = rnf pl

-- | A convenience alias for use with 'resultToEither' to avoid having
--   to type the entire type out.
type EitherResult s a = Either (ParsingErrors s) a

-- | Convert the result into an 'Either' value.
resultToEither :: (ParseInput s) => Result s a -> (EitherResult s a, s)
resultToEither (Success s a)     = (Right a, s)
resultToEither (Failure s pl)    = (Left pl, s)
resultToEither (Partial pl _cnt) = (Left pl, fromStream mempty)

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
          ParseState s     -- The input and stack trace log.
          -> Failure s   r -- What to do when we fail.
          -> Success s a r -- What to do when we succeed.
          -> Result  s   r
  }

{-

How the parser works
--------------------

Traditionally, the way a parser works has been "bottom-up": a
sub-parser is evaluated, returning a @Result@ value; if it fails do
one thing, if it succeeds do another.

Instead, for this parser the caller specifies what the sub-parser
should do if it reaches a failure or when it succeeds.

Since function composition/calling is more efficient than continually
doing comparisons on data types, this results in _speed_!

In terms of usage, this is completely opaque to the user of the
parsing library: they are not specifically asked for any functions for
dealing with success or failure, etc.

Constructing a low-level parser typically looks like:

> ... = P $ \ inp add mr adjE fl sc -> ...
              |   |   |  |    |  |
              |   |   |  |    |  |
              |   |   |  |    |  \-- What to do on a successful parse.
              |   |   |  |    |
              |   |   |  |    \----- What to do if the parse isn't successful.
              |   |   |  |
              |   |   |  \---------- Function to apply on error messages.
              |   |   |
              |   |   \------------- Whether we are accepting any more input.
              |   |
              |   \----------------- Any additional input we have received so far.
              |
              \--------------------- Input to the parser.

Note that the 'AdjErr' argument is at the top-level of the parser
rather than directly manipulating the arguments to the 'Failure'
function due to how 'commit' works.

-}

-- The stateful part of parsing: rather than separate values, we can
-- improve performance - and readability - by having one state
-- variable to cover the input, additional input and whether there is
-- any more input expected.
--
-- CONVENTION: a value of this type is called something like @pSt@.
data ParseState s = PSt { input       :: !s
                          -- ^ The input we're currently parsing.
                        , add         ::  Stream s
                          -- ^ Any additional input we may have
                          --   received since we started parsing; used
                          --   by onFail.
                        , more        :: !More
                        , errLog      :: ParseLog s
                        , isCommitted :: !Bool
                        }

deriving instance (TokenStream s, Eq   s, Eq   (Stream s), Eq   (Token s)) => Eq   (ParseState s)
deriving instance (TokenStream s, Show s, Show (Stream s), Show (Token s)) => Show (ParseState s)

-- Have we read all available input?
--
-- CONVENTION: a value of this type is called something like @mr@.
data More = Complete | Incomplete
            deriving (Eq, Ord, Show, Read)

instance Monoid More where
  mempty = Incomplete

  mappend c@Complete _ = c
  mappend _          m = m

-- What to do when we fail a parse; @s@ is the input type.
--
-- CONVENTION: a value of this type is called something like @fl@.
type Failure s   r = ParseState s -> ParseError s -> Result s r

-- What to do when a parse is successful; @s@ is the input type, @a@
-- is the result of the parse, @r@ is the output type.
--
-- CONVENTION: a value of this type is called something like @sc@.
type Success s a r = ParseState s -> a -> Result s r

-- Dum... Dum... Dum... DUMMMMMM!!!  The parsing has gone all wrong,
-- so apply the error-message adjustment and stop doing anything.
failure :: Failure s r
failure pSt e = Failure (input pSt) (createLogFrom pSt e)
{-# INLINE failure #-}

createLogFrom :: ParseState s -> ParseError s -> ParsingErrors s
createLogFrom pSt e = createFinalLog (errLog pSt) e (input pSt)
{-# INLINE createLogFrom #-}

-- Hooray!  We're all done here, and a job well done!
successful :: Success s a a
successful pSt = Success (input pSt)
{-# INLINE successful #-}

makeState :: (ParseInput s) => Stream s -> More -> ParseState s
makeState inp m = PSt { input       = fromStream inp
                      , add         = mempty
                      , more        = m
                      , errLog      = mempty
                      , isCommitted = False
                      }
{-# INLINE makeState #-}

-- | Run the parser on the provided input, providing the raw 'Result'
--   value.
parseInput :: (ParseInput s) => Parser s a -> Stream s -> Result s a
parseInput p inp = runP p (makeState inp Incomplete) failure successful
{-# INLINE parseInput #-}

-- | Run a parser.
runParser :: (ParseInput s) => Parser s a -> Stream s -> (EitherResult s a, s)
runParser p inp = resultToEither (runP p (makeState inp Complete) failure successful)
{-# INLINE runParser #-}

-- | Run a parser, assuming it succeeds.  If the parser fails, use
--   'error' to display the message.
runParser' :: (ParseInput s) => Parser s a -> Stream s -> a
runParser' p inp = case fst $ runParser p inp of
                     Right a -> a
                     Left pl -> error ('\n' : prettyLog pl)

-- -----------------------------------------------------------------------------
-- Error Handling

-- | Fail with a specific error.  When @OverloadedStrings@ is enabled,
--   this becomes equivalent to 'fail' (at least for literal 'String's).
failWith :: ParseError s -> Parser s a
failWith e = P $ \ pSt fl _sc -> fl pSt e
{-# INLINE failWith #-}

-- | A convenient function to produce (reasonably) pretty stack traces
--   for parsing failures.
addStackTrace :: ParseError s -> Parser s a -> Parser s a
addStackTrace e p = P $ \ pSt fl sc ->
  runP p (pSt { errLog = logError (errLog pSt) e (input pSt) }) fl sc
{-# INLINE addStackTrace #-}

-- | Name the parser, as a shorter variant of specifying a longer
--   error message.
(<?>) :: Parser s a -> String -> Parser s a
p <?> f = addStackTrace (ParserName f) p
{-# INLINE (<?>) #-}
infix 0 <?>

-- -----------------------------------------------------------------------------
-- Instances

instance Functor (Parser s) where
  fmap = fmapP
  {-# INLINE fmap #-}

fmapP :: (a -> b) -> Parser s a -> Parser s b
fmapP f pa = P $ \ pSt fl sc ->
                 runP pa pSt fl $ \ pSt' a -> sc pSt' (f a)
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

returnP :: a -> Parser s a
returnP a = P $ \ pSt _fl sc -> sc pSt a
{-# INLINE returnP #-}

-- Explicit version of @pa >>= const pb@.
ignFirstP :: Parser s a -> Parser s b -> Parser s b
ignFirstP pa pb = P $ \ pSt fl sc ->
                        runP pa pSt fl $ \ pSt' _a
                          -> runP pb pSt' fl sc
{-# INLINE ignFirstP #-}

-- Commit isn't propagated here... and it really should be.
-- Dammit, how can we do this?

discard :: Parser s a -> Parser s b -> Parser s a
discard pa pb = P $ \ pSt fl sc ->
                  let sc' a pSt' b = b `seq` sc pSt' a
                      -- Ignore the provided result and use the one
                      -- you obtained earlier.
                  in runP pa pSt fl $ \ pSt' a ->
                       runP pb pSt' fl (sc' a)
{-# INLINE discard #-}

apP :: Parser s (a -> b) -> Parser s a -> Parser s b
apP pf pa = P $ \ pSt fl sc ->
                  runP pf pSt fl $ \ pSt' f ->
                    runP pa pSt' fl $ \ pSt'' a ->
                      sc pSt'' (f a)
{-# INLINE apP #-}

instance (ParseInput s) => Alternative (Parser s) where
  empty = failP "empty"
  {-# INLINE empty #-}

  (<|>) = onFail
  {-# INLINE (<|>) #-}

  many v = many_v <?> "many"
    where
      many_v = some_v <|> pure []
      some_v = do a <- v
                  commitNoLog ((a:) <$> many_v)
  {-# INLINE many #-}

  some v = some_v <?> "some"
    where
      many_v = some_v <|> pure []
      some_v = do a <- v
                  commitNoLog ((a:) <$> many_v)
  {-# INLINE some #-}

onFail :: (ParseInput s) => Parser s a -> Parser s a -> Parser s a
onFail p1 p2 = wrapCommitment $ P $ \ pSt fl sc ->
  let fl' pSt' e
          | isCommitted pSt' = failure (restoreAdd pSt') e
          | otherwise        = let lg = completeLog $ createLogFrom pSt' e
                                   p2' = addStackTrace (Backtrack lg) p2
                               in mergeIncremental pSt pSt' $
                                    \ pSt'' -> runP p2' pSt'' fl sc
      -- If we fail - and aren't committed - run parser
      -- p2 instead.  We need to ensure that if p1
      -- requested and obtained additional input that we
      -- use it as well.

      sc' pSt' = sc (restoreAdd pSt')

      -- Put back in the original additional input and error log.
      restoreAdd pSt' = pSt' { add    = add    pSt <> add    pSt'
                             , errLog = errLog pSt <> errLog pSt'
                             }

  in ignoreAdditional pSt $ \ pSt'
       -> runP p1 pSt' fl' sc'
      -- We want to be able to differentiate the additional values
      -- that we already have vs any we may get from running @p1@.
{-# INLINE onFail #-}


-- Used when you temporarily want to assume that a parser isn't
-- committed, usually because you want to see if a component makes it
-- committed.
wrapCommitment :: Parser s a -> Parser s a
wrapCommitment p = P $ \ pSt fl sc ->
  let origCommit = isCommitted pSt
      fl' pSt' = fl (pSt' { isCommitted = origCommit || isCommitted pSt' })
      sc' pSt' = sc (pSt' { isCommitted = origCommit || isCommitted pSt' })
  in runP p (pSt { isCommitted = False }) fl' sc'
{-# INLINE wrapCommitment #-}


instance Monad (Parser s) where
  return = returnP
  {-# INLINE return #-}

  (>>=) = bindP
  {-# INLINE (>>=) #-}

  (>>) = ignFirstP
  {-# INLINE (>>) #-}

  fail = failP
  {-# INLINE fail #-}

failP :: String -> Parser s a
failP = failWith . Message
{-# INLINE failP #-}

bindP ::  Parser s a -> (a -> Parser s b) -> Parser s b
bindP p f = P $ \ pSt fl sc -> runP p pSt fl $
                 -- Get the new parser and run it.
                  \ pSt' a -> runP (f a) pSt' fl sc
{-# INLINE bindP #-}

instance (ParseInput s) => MonadPlus (Parser s) where
  mzero = failP "mzero"
  {-# INLINE mzero #-}

  mplus = onFail
  {-# INLINE mplus #-}

-- -----------------------------------------------------------------------------
-- Commitment

-- For internal use only (as it's used in the definitions of many and
-- some, and we want to cut down on noise).  See 'commit' from
-- Text.Paraphrase instead.
--
-- Note that we only keep the additional input around in case we
-- backtrack and need to restore what was added when going to a
-- different branch.  As such, since commit doesn't allow
-- backtracking, there's no point keeping the additional input around
-- as it won't get used!.
commitNoLog :: (ParseInput s) => Parser s a -> Parser s a
commitNoLog p = P $ \ pSt fl sc ->
  let  pSt' = pSt { add         = mempty
                  , isCommitted = True
                  }
  in runP p pSt' fl sc
{-# INLINE commitNoLog #-}

-- -----------------------------------------------------------------------------
-- Some basic parsers

get :: Parser s s
get = P $ \ pSt _fl sc -> sc pSt (input pSt)
{-# INLINE get #-}

put :: s -> Parser s ()
put s = P $ \ pSt _fl sc -> sc (pSt { input = s }) ()
{-# INLINE put #-}

-- -----------------------------------------------------------------------------
-- Incremental support

-- @mergeIncremental inc1 inc2@ is used when @inc2@ originally started
-- as having the same 'received' input as @inc1@, but may have since
-- received additional input.
mergeIncremental :: (ParseInput s) => ParseState s -> ParseState s
                    -> (ParseState s -> r) -> r
mergeIncremental pSt1 pSt2 f =
  let !pSt = pSt1 { input = input pSt1 `appendStream` add  pSt2
                  , add   = add   pSt1 <>             add  pSt2
                  , more  = more  pSt1 <>             more pSt2
                  }
                  -- We only want the original log, not what may have
                  -- happened since they split.
  in f pSt
{-# INLINE mergeIncremental #-}

-- A wrapper to set the additional input and error log to be empty as
-- we want to know solely what input and errors _this_ parser obtains.
ignoreAdditional :: (ParseInput s) => ParseState s -> (ParseState s -> r) -> r
ignoreAdditional pSt f = f (pSt { add = mempty, errLog = mempty })
{-# INLINE ignoreAdditional #-}
