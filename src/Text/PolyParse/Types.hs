{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies
             #-}
{- |
   Module      : Text.PolyParse.Types
   Description : Definition of types
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Definition of types, classes and low-level functions.

 -}
module Text.PolyParse.Types where

import Text.PolyParse.TextManipulation

import Control.Applicative
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
          WithIncremental s   -- The input.
            (AdjErr           -- What to do with any error messages.
             -> Failure s   r -- What to do when we fail.
             -> Success s a r -- What to do when we succeed.
             -> Result  s   r)
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

> ... = P $ \ inp adjE fl sc -> ...
              |   |    |  |
              |   |    |  |
              |   |    |  \-- What to do on a successful parse.
              |   |    |
              |   |    \----- What to do if the parse isn't successful.
              |   |
              |   \---------- Function to apply on error messages.
              |
              \-------------- Input to the parser.

Note that the 'AdjErr' argument is at the top-level of the parser
rather than directly manipulating the arguments to the 'Failure'
function due to how 'commit' works.

-}

-- An alias to make types involving explicit incremental support
-- easier to read.
type WithIncremental s r = Input s -> Additional s -> More -> r

-- The input that we're currently parsing.
--
-- CONVENTION: a value of this type is called something like @inp@.
newtype Input s = I { unI :: s }
                  deriving (Eq, Ord, Show, Read, Monoid)

-- Any additional input that has been provided.  Note that we do not
-- explicitly parse through this value; it is used in functions like
-- 'mergeIncremental' where we are considering what to do when one
-- parser may have requested (and received) more input and thus need
-- to add in the extra provided input.
--
-- CONVENTION: a value of this type is called something like @add@.
newtype Additional s = A { unA :: s }
                       deriving (Eq, Ord, Show, Read, Monoid)

-- Have we read all available input?
--
-- CONVENTION: a value of this type is called something like @mr@.
data More = Complete | Incomplete
            deriving (Eq, Ord, Show, Read)

instance Monoid More where
  mempty = Incomplete

  mappend c@Complete _ = c
  mappend _          m = m
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
type Failure s   r = WithIncremental s (AdjErr -> ErrMsg -> Result s r)

-- What to do when a parse is successful; @s@ is the input type, @a@
-- is the result of the parse, @r@ is the output type.
--
-- CONVENTION: a value of this type is called something like @sc@.
type Success s a r = WithIncremental s (a -> Result s r)

-- Start the difference list by doing nothing.
noAdj :: AdjErr
noAdj = id

-- Dum... Dum... Dum... DUMMMMMM!!!  The parsing has gone all wrong,
-- so apply the error-message adjustment and stop doing anything.
failure :: Failure s r
failure inp _add _mr adjE e = Failure (unI inp) (adjE $ indMsg e)
  where
    indMsg = allButFirstLine (indent lenStackTracePoint)

-- Hooray!  We're all done here, and a job well done!
successful :: Success s a a
successful inp _add _mr = Success (unI inp)

-- | Run the parser on the provided input, providing the raw 'Result'
--   value.
parseInput :: (ParseInput s) => Parser s a -> s -> Result s a
parseInput p inp = runP p (I inp) mempty Incomplete noAdj failure successful

-- | Run a parser.
runParser :: (ParseInput s) => Parser s a -> s -> (Either String a, s)
runParser p inp = resultToEither
                    (runP p (I inp) mempty Complete noAdj failure successful)

-- | Run a parser, assuming it succeeds.  If the parser fails, use
--   'error' to display the message.
runParser' :: (ParseInput s) => Parser s a -> s -> a
runParser' p inp = case fst $ runParser p inp of
                     Right a  -> a
                     Left err -> error ('\n':err)

-- -----------------------------------------------------------------------------
-- Instances

instance Functor (Parser s) where
  fmap = fmapP
  {-# INLINE fmap #-}

fmapP :: (a -> b) -> Parser s a -> Parser s b
fmapP f pa = P $ \ inp add mr adjE fl sc ->
                 runP pa inp add mr adjE fl $ \ inp' add' mr' a ->
                                                 sc inp' add' mr' (f a)
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

instance (ParseInput s) => Alternative (Parser s) where
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

instance (ParseInput s) => MonadPlus (Parser s) where
  mzero = failP "mzero"
  {-# INLINE mzero #-}

  mplus = onFail
  {-# INLINE mplus #-}

returnP :: a -> Parser s a
returnP a = P $ \ inp add mr _adjE _fl sc -> sc inp add mr a
{-# INLINE returnP #-}

-- Explicit version of @pa >>= const pb@.
ignFirstP :: Parser s a -> Parser s b -> Parser s b
ignFirstP pa pb = P $ \ inp add mr adjE fl sc ->
                        runP pa inp add mr adjE fl $ \ inp' add' mr' _a ->
                                                runP pb inp' add' mr' adjE fl sc
{-# INLINE ignFirstP #-}

discard :: Parser s a -> Parser s b -> Parser s a
discard pa pb = P $ \ inp add mr adjE fl sc ->
                  let sc' a inp' add' mr' b = b `seq` sc inp' add' mr' a
                      -- Ignore the provided result and use the one
                      -- you obtained earlier.
                  in runP pa inp add mr adjE fl $ \ inp' add' mr' a ->
                                              runP pb inp' add' mr' adjE fl (sc' a)
{-# INLINE discard #-}

apP :: Parser s (a -> b) -> Parser s a -> Parser s b
apP pf pa = P $ \ inp add mr adjE fl sc ->
                  runP pf inp add mr adjE fl
                       $ \ inp' add' mr' f -> runP pa inp' add' mr' adjE fl
                                          $ \ inp'' add'' mr'' a -> sc inp'' add'' mr'' (f a)
{-# INLINE apP #-}

failP :: String -> Parser s a
failP e = P $ \ inp add mr adjE fl _sc -> fl inp add mr adjE e
{-# INLINE failP #-}

bindP ::  Parser s a -> (a -> Parser s b) -> Parser s b
bindP p f = P $ \ inp add mr adjE fl sc -> runP p inp add mr adjE fl $
                 -- Get the new parser and run it.
                  \ inp' add' mr' a -> runP (f a) inp' add' mr' adjE fl sc
{-# INLINE bindP #-}

onFail :: (ParseInput s) => Parser s a -> Parser s a -> Parser s a
onFail p1 p2 = P $ \ inp add mr adjE fl sc ->
               let fl' inp' add' mr' _adjE' _e
                       = mergeIncremental inp add mr inp' add' mr' $
                         \ inp'' add'' mr'' -> runP p2 inp'' add'' mr'' adjE fl sc
                   -- If we fail, run parser p2 instead.  Don't use
                   -- the provided @AdjErr@ value, get the "global"
                   -- one instead (as we don't want p1's stack
                   -- traces).  We also need to ensure that if p1
                   -- requested and obtained additional input that we
                   -- use it as well.

                   sc' inp' add' mr' = sc inp' (add <> add') mr'
                   -- Put back in the original additional input.
             in ignoreAdditional inp add mr $
                 -- We want to be able to differentiate the
                 -- 'Additional' value that we already have vs any we
                 -- may get from running @p1@.
                  \ inp' add' mr' -> runP p1 inp' add' mr' adjE fl' sc'
{-# INLINE onFail #-}

-- -----------------------------------------------------------------------------
-- Incremental support

-- If we pretend that the three parts of the 'WithIncremental' inputs
-- are one value: @mergeIncremental inc1 inc2@ is used when @inc2@
-- originally started as having the same 'received' input as @inc1@,
-- but may have since received additional input.
mergeIncremental :: (Monoid s) => WithIncremental s
                                    (WithIncremental s
                                      (WithIncremental s r -> r))
mergeIncremental inp1 add1 mr1 _inp2 add2 mr2 f =
  let !inp = inp1 <> I (unA add2)
      !add = add1 <> add2
      !mr  = mr1  <> mr2
  in f inp add mr
{-# INLINE mergeIncremental #-}

ignoreAdditional :: (Monoid s) => WithIncremental s (WithIncremental s r -> r)
ignoreAdditional inp _add mr f = f inp mempty mr
{-# INLINE ignoreAdditional #-}