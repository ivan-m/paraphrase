{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies
             #-}
{- |
   Module      : Text.Paraphrase.Types
   Description : Definition of types
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Definition of types, classes and low-level functions.

 -}
module Text.Paraphrase.Types where

import Text.Paraphrase.TextManipulation

import Control.Applicative
import Control.Arrow       ((***))
import Control.DeepSeq     (NFData (rnf))
import Control.Monad       (MonadPlus (..))
import Data.Monoid

import qualified Data.ByteString            as SB
import           Data.ByteString.Char8      ()
import           Data.ByteString.Internal   (w2c)
import qualified Data.ByteString.Lazy       as LB
import           Data.ByteString.Lazy.Char8 ()
import qualified Data.ByteString.Unsafe     as SB
import           Data.String                (IsString (..))
import qualified Data.Text                  as ST
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Unsafe           as ST
import           Data.Word                  (Word8)

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

  -- | The elements of this input type.
  type Token s

  -- | Obtain the first token in the input.  Only used when the input
  --   isn't empty, honest!
  inputHead :: s -> Token s

  -- | Return all but the first token of the input.  Only used when
  --   the input isn't empty, honest!
  inputTail :: s -> s

  -- | Is the input empty?
  isEmpty :: s -> Bool
  isEmpty = not . (`lengthAtLeast` 1)

  -- | Do we have at least @n@ tokens available?
  lengthAtLeast :: s -> Int -> Bool

  -- | Split the stream where the predicate is no longer satisfied
  --   (that is, the @fst@ component contains the largest possible
  --   prefix where all values satisfy the predicate, and the @snd@
  --   component contains the latter).
  breakWhen :: (Token s -> Bool) -> s -> (s,s)

instance ParseInput [a] where
  type Token [a] = a

  inputHead = head

  inputTail = tail

  isEmpty = null

  lengthAtLeast as n = not . null . drop (n-1) $ as
  {-# INLINE lengthAtLeast #-}

  breakWhen = span

instance ParseInput SB.ByteString where
  type Token SB.ByteString = Word8

  inputHead = SB.unsafeHead

  inputTail = SB.unsafeTail

  isEmpty = SB.null

  -- length is O(1)
  lengthAtLeast bs n = SB.length bs >= n
  {-# INLINE lengthAtLeast #-}

  breakWhen = SB.span

instance ParseInput LB.ByteString where
  type Token LB.ByteString = Word8

  inputHead = LB.head

  inputTail = LB.tail

  isEmpty = LB.null

  -- length is O(n)
  lengthAtLeast bs n = LB.length bs >= fromIntegral n
  {-# INLINE lengthAtLeast #-}

  breakWhen = LB.span

instance ParseInput ST.Text where
  type Token ST.Text = Char

  inputHead = ST.unsafeHead

  inputTail = ST.unsafeTail

  isEmpty = ST.null

  -- We do @`quot` 2@ because UTF-16 (which Text is implemented with)
  -- code points are either 1 or 2 Word16 values.  As such do this
  -- O(1) test first in case it suffices before we do the O(n) case
  -- for the real length.
  lengthAtLeast t n = (ST.lengthWord16 t `quot` 2) >= n || ST.length t >= n
  {-# INLINE lengthAtLeast #-}

  breakWhen = ST.span

instance ParseInput LT.Text where
  type Token LT.Text = Char

  inputHead = LT.head

  inputTail = LT.tail

  isEmpty = LT.null

  -- Doesn't seem to be any real alternative but to do the O(n)
  -- length.
  lengthAtLeast t n = LT.length t >= fromIntegral n
  {-# INLINE lengthAtLeast #-}

  breakWhen = LT.span

newtype AsChar8 s = AsChar8 { unChar8 :: s }
                    deriving (Eq, Ord, Show, Read, IsString, Monoid, NFData)

instance (ParseInput s, Token s ~ Word8) => ParseInput (AsChar8 s) where
  type Token (AsChar8 s) = Char

  inputHead = w2c . inputHead . unChar8

  inputTail = AsChar8 . inputTail . unChar8

  isEmpty = isEmpty . unChar8

  lengthAtLeast s = lengthAtLeast (unChar8 s)
  {-# INLINE lengthAtLeast #-}

  breakWhen f = (AsChar8 *** AsChar8) . breakWhen (f . w2c) . unChar8

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
                | Failure s AdjustError String
                | Partial AdjustError (s -> Result s a)
                  -- ^ Indicates that the parser requires more input
                  --   to continue.
                  --
                  --   The 'AdjustError' is for use with
                  --   'resultToEither' and can be safely ignored in
                  --   your own code.

-- | Nonsensical for 'Failure' and 'Partial'.
instance (Show s, Show a) => Show (Result s a) where
  showsPrec d r = showParen (d > 10) $
                    case r of
                      Success s a       -> showString "Success "
                                           . shows s
                                           . showString " "
                                           . shows a
                      Failure s adjE e -> showString "Failure "
                                          . shows s
                                          . showString " "
                                          . shows adjE
                                          . showString " "
                                          . shows e
                      Partial adjE _p  -> showString "Partial "
                                          . shows adjE
                                          . showString " "
                                          . showString "\"<continuation>\""

instance Functor (Result s) where
  fmap f (Success s a)      = Success s (f a)
  fmap _ (Failure s adjE e) = Failure s adjE e
  fmap f (Partial adjE cnt) = Partial adjE (fmap f . cnt)

instance (NFData s, NFData a) => NFData (Result s a) where
  rnf (Success s a)      = rnf s `seq` rnf a
  rnf (Failure s adjE e) = rnf s `seq` rnf adjE `seq` rnf e
  rnf (Partial adjE _)   = rnf adjE

-- | A transformation on an error message to get any additional
--   messages provided by combinators (e.g. to provide a stack trace).
newtype AdjustError = AE { adjustError :: String -> String
                           -- ^ Adjust an error message to obtain any
                           --   additional stack traces, etc. that may
                           --   be available.
                         }

-- | A nonsensical definition just to allow data types containing this
--   value to have valid @Show@ instances.
instance Show AdjustError where
  show _ = "\"<AdjustError>\""

-- Define an instance in case we change the type later.
instance NFData AdjustError where
  rnf _ = ()

-- | A convenience alias for use with 'resultToEither' to avoid having
--   to type the entire type out.
type EitherResult a = Either (AdjustError, String) a

-- | Convert the result into an 'Either' value.
resultToEither :: (ParseInput s) => Result s a -> (EitherResult a, s)
resultToEither (Success s a)       = (Right a, s)
resultToEither (Failure s adjE e)  = (Left (adjE,e), s)
resultToEither (Partial adjE _cnt) = let e = "More input required."
                                     in (Left (adjE,e),mempty)

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
            (   Failure s   r -- What to do when we fail.
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

-- An alias to make types involving explicit incremental support
-- easier to read.
type WithIncremental s r = ParseState s -> r -- Input s -> Additional s -> More -> r

-- The stateful values for parsing.  One large value is used rather
-- than separating them to help make more manageable and readable.
--
-- CONVENTION: a value of this type is called something like @pSt@.
data ParseState s = PS { input      :: !(Input s)
                       , additional :: !(Additional s)
                       , more       :: !More
                       , parseLog   :: !AdjErr
                       }

instance (Show s) => Show (ParseState s) where
  showsPrec d pSt = showParen (d > 10) $
    showString "PS { input = "
    . shows (input pSt)
    . showString ", additional = "
    . shows (additional pSt)
    . showString ", more = "
    . shows (more pSt)
    . showString ", parseLog = \"<error log>\" }"


blankState :: (Monoid s) => ParseState s
blankState = PS { input      = mempty
                , additional = mempty
                , more       = Incomplete
                , parseLog   = noAdj
                }

completeState :: (Monoid s) => s -> ParseState s
completeState inp = blankState { input = I inp
                               , more  = Complete
                               }

incompleteState :: (Monoid s) => s -> ParseState s
incompleteState inp = blankState { input = I inp
                                 -- Not to rely upon current coding default
                                 , more  = Incomplete
                                 }

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
type Failure s   r = WithIncremental s (ErrMsg -> Result s r)

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
failure pSt e = Failure (unI $ input pSt) addE' e
  where
    addE' = AE $ parseLog pSt . indMsg
    indMsg = allButFirstLine (indent lenStackTracePoint)

-- Hooray!  We're all done here, and a job well done!
successful :: Success s a a
successful pSt = Success (unI $ input pSt)

-- | Run the parser on the provided input, providing the raw 'Result'
--   value.
parseInput :: (ParseInput s) => Parser s a -> s -> Result s a
parseInput p inp = runP p (incompleteState inp) failure successful

-- | Run a parser.
runParser :: (ParseInput s) => Parser s a -> s
             -> (EitherResult a, s)
runParser p inp = resultToEither
                    (runP p (completeState inp) failure successful)

-- | Run a parser, assuming it succeeds.  If the parser fails, use
--   'error' to display the message.
runParser' :: (ParseInput s) => Parser s a -> s -> a
runParser' p pSt = case fst $ runParser p pSt of
                     Right a       -> a
                     Left (adjE,e) -> error ('\n' : adjustError adjE e)

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
                        runP pa pSt fl $ \ pSt' _a -> runP pb pSt' fl sc
{-# INLINE ignFirstP #-}

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
                  runP pf pSt fl
                       $ \ pSt' f -> runP pa pSt' fl
                                          $ \ pSt'' a -> sc pSt'' (f a)
{-# INLINE apP #-}

instance (ParseInput s) => Alternative (Parser s) where
  empty = failP "empty"
  {-# INLINE empty #-}

  (<|>) = onFail
  {-# INLINE (<|>) #-}

  many v = many_v
    where
      many_v = some_v <|> pure []
      some_v = (:) <$> v <*> many_v
  {-# INLINE many #-}

  some v = some_v
    where
      many_v = some_v <|> pure []
      some_v = (:) <$> v <*> many_v
  {-# INLINE some #-}

onFail :: (ParseInput s) => Parser s a -> Parser s a -> Parser s a
onFail p1 p2 = P $ \ pSt fl sc ->
               let fl' pSt' _e
                       = mergeIncremental pSt pSt' $
                         \ pSt'' -> runP p2 pSt'' fl sc
                   -- If we fail, run parser p2 instead.  Don't use
                   -- the provided @AdjErr@ value, get the "global"
                   -- one instead (as we don't want p1's stack
                   -- traces).  We also need to ensure that if p1
                   -- requested and obtained additional input that we
                   -- use it as well.

                   sc' pSt' = sc (prependAdditional pSt pSt')
                   -- Put back in the original additional input.
             in runP p1 (ignoreAdditional pSt) fl' sc'
                 -- We want to be able to differentiate the
                 -- 'Additional' value that we already have vs any we
                 -- may get from running @p1@.
{-# INLINE onFail #-}

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
failP e = P $ \ pSt fl _sc -> fl pSt e
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
-- Incremental support

-- @mergeIncremental inc1 inc2@ is used when @inc2@ originally started
-- as having the same 'received' input as @inc1@, but may have since
-- received additional input.
mergeIncremental :: (Monoid s) => WithIncremental s
                                    (WithIncremental s
                                      (WithIncremental s r -> r))
mergeIncremental pSt1 pSt2 f =
  let !i = input pSt1 <> I (unA $ additional pSt2)
      !a = additional pSt1 <> additional pSt2
      !m = more pSt1  <> more pSt2
  in f (pSt1 { input = i, additional = a, more = m })
{-# INLINE mergeIncremental #-}

-- Used when - to start with - @pSt2 = ignoreAdditional inp1@, but
-- then possibly had input consumed and additional input obtained.  As
-- such, add the original additional value back in.
prependAdditional :: (Monoid s) => ParseState s -> ParseState s -> ParseState s
prependAdditional pSt1 pSt2 = pSt2 { additional = additional pSt1 <> additional pSt2 }
{-# INLINE prependAdditional #-}

ignoreAdditional :: (Monoid s) => ParseState s -> ParseState s
ignoreAdditional pSt = pSt { additional = mempty }
{-# INLINE ignoreAdditional #-}

-- | Optimise for the common case of ensuring there's at least one
--   token, as isEmpty is usually O(1) whilst lengthAtLeast might be
--   O(n).
checkLength :: (ParseInput s) => s -> Int -> Bool
checkLength s 1 = not $ isEmpty s
checkLength s n = lengthAtLeast s n
{-# INLINE checkLength #-}

-- | Make sure that there are at least @n@ 'Token's available.
needAtLeast :: (ParseInput s) => Int -> Parser s ()
needAtLeast !n = go
  where
    go = P $ \ pSt fl sc ->
      if checkLength (unI $ input pSt) n
         then sc pSt ()
         else runP (needMoreInput *> go) pSt fl sc
{-# INLINE needAtLeast #-}

ensure :: (ParseInput s) => Int -> Parser s s
ensure !n = P $ \ pSt fl sc ->
      if checkLength (unI $ input pSt) n
         then sc pSt (unI $ input pSt)
         else ensure' n pSt fl sc
{-# INLINE ensure #-}

-- The un-common case is split off to avoid recursion in ensure, so
-- that it can be inlined properly.
ensure' :: (ParseInput s) => Int -> WithIncremental s (Failure s   r -> Success s s r -> Result  s   r)
ensure' !n pSt fl sc = runP (needMoreInput *> go n) pSt fl sc
  where
    go !n' = P $ \ pSt' fl' sc' ->
      if checkLength (unI $ input pSt') n'
         then sc' pSt' (unI $ input pSt')
         else runP (needMoreInput *> go n') pSt' fl' sc'

put :: s -> Parser s ()
put s = P $ \ pSt _fl sc -> sc (pSt { input = I s }) ()

-- | Request more input.
needMoreInput :: (ParseInput s) => Parser s ()
needMoreInput = P $ \ pSt fl sc ->
  if more pSt == Complete
     then fl pSt "Not enough input."
     else let fl' pSt' = fl pSt' "Not enough input."
              sc' pSt' = sc pSt' ()
          in requestInput pSt fl' sc'

-- | Construct a 'Partial' 'Result' with a continuation function that
--   will use the first provided function if it fails, and the second
--   if it succeeds.
--
--   It is assumed that @more pSt == Incomplete@.
requestInput :: (ParseInput s) =>
                WithIncremental s
                  (    WithIncremental s (Result s r) -- Failure case
                    -> WithIncremental s (Result s r) -- Success case
                    -> Result s r)
requestInput pSt fl sc = Partial (AE (parseLog pSt)) $ \ s ->
  if isEmpty s
     then fl (pSt { more = Complete })
     else sc (pSt { input = input pSt <> I s, additional = additional pSt <> A s})
