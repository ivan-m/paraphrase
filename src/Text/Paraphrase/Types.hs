{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances, RankNTypes,
             StandaloneDeriving, TypeFamilies, UndecidableInstances #-}
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
import Text.Paraphrase.Pretty (PrettyValue)
import Text.Paraphrase.Stack

import           Control.Applicative
import           Control.Arrow       (second)
import           Control.DeepSeq     (NFData (rnf))
import           Control.Monad       (MonadPlus (..))
import qualified Data.ListLike       as LL
import           Data.Monoid
import           Data.String         (IsString (..))

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
data Result e s a = Success (Stream s) a
                  | Failure (Stream s) (ParsingErrors e s)
                  | Partial            (ParsingErrors e s) (Stream s -> Result e s a)
                    -- ^ Indicates that the parser requires more input
                    --   to continue.
                    --
                    --   The 'ParseError' is for use with
                    --   'resultToEither' and can be safely ignored in
                    --   your own code.

-- | Nonsensical for 'Partial'.
instance (Show e, ParseInput s, Show s, Show (Stream s), Show (Token s), Show a) => Show (Result e s a) where
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

instance Functor (Result e s) where
  fmap f (Success s a)    = Success s (f a)
  fmap _ (Failure s pl)   = Failure s pl
  fmap f (Partial pl cnt) = Partial pl (fmap f . cnt)

instance (NFData e, ParseInput s, NFData s, NFData (Stream s), NFData (Token s), NFData a)
         => NFData (Result e s a) where
  rnf (Success s a)  = rnf s `seq` rnf a
  rnf (Failure s pl) = rnf s `seq` rnf pl
  rnf (Partial pl _) = rnf pl

-- | A convenience alias for use with 'resultToEither' to avoid having
--   to type the entire type out.
type EitherResult e s a = Either (ParsingErrors e s) a

-- | Convert the result into an 'Either' value.
resultToEither :: (ParseInput s) => Result e s a -> (EitherResult e s a, Stream s)
resultToEither (Success s a)     = (Right a, s)
resultToEither (Failure s pl)    = (Left pl, s)
resultToEither (Partial pl _cnt) = (Left pl, mempty)

-- | Change the custom error type.  Useful if you want to chain an
--   existing parser into your own parser where the error type is
--   different.
changeError :: (e -> e') -> Result e s r -> Result e' s r
changeError _ (Success s  a)   = Success s a
changeError f (Failure s  pl)  = Failure s (convertErrorBy f pl)
changeError f (Partial pl cnt) = Partial   (convertErrorBy f pl)
                                           (changeError f . cnt)
{-# INLINE changeError #-}

changeErrorE :: (e -> e') -> EitherResult e s a -> EitherResult e' s a
changeErrorE f (Left pes) = Left (convertErrorBy f pes)
changeErrorE _ (Right a)  = Right a
{-# INLINE changeErrorE #-}

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
newtype Parser e s a = P {
  -- Our parser is actually a function of functions!
  runP :: forall r.
          ParseState e s     -- The input and stack trace log.
          -> Failure e s   r -- What to do when we fail.
          -> Success e s a r -- What to do when we succeed.
          -> Result  e s   r
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
data ParseState e s = PSt { input       :: !s
                            -- ^ The input we're currently parsing.
                          , add         ::  Stream s
                            -- ^ Any additional input we may have
                            --   received since we started parsing; used
                            --   by onFail.
                          , more        :: !More
                          , errLog      :: Stack (ParseLog e s)
                          , isCommitted :: !Bool
                          }

deriving instance (TokenStream s, Eq   s, Eq   (Stream s), Eq   (ParseLog e s)) => Eq   (ParseState e s)
deriving instance (TokenStream s, Show s, Show (Stream s), Show (ParseLog e s)) => Show (ParseState e s)

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
type Failure e s   r = ParseState e s -> ParseError e s -> Result e s r

-- What to do when a parse is successful; @s@ is the input type, @a@
-- is the result of the parse, @r@ is the output type.
--
-- CONVENTION: a value of this type is called something like @sc@.
type Success e s a r = ParseState e s -> a -> Result e s r

-- Dum... Dum... Dum... DUMMMMMM!!!  The parsing has gone all wrong,
-- so apply the error-message adjustment and stop doing anything.
failure :: (ParseInput s) => Failure e s r
failure pSt e = Failure (getStream $ input pSt) (createLogFrom pSt e)
{-# INLINE failure #-}

createLogFrom :: ParseState e s -> ParseError e s -> ParsingErrors e s
createLogFrom pSt e = createFinalLog (mergedLog pSt) e (input pSt)
{-# INLINE createLogFrom #-}

-- Hooray!  We're all done here, and a job well done!
successful :: (ParseInput s) => Success e s a a
successful pSt = Success (getStream $ input pSt)
{-# INLINE successful #-}

makeState :: (ParseInput s) => Stream s -> More -> ParseState e s
makeState = makeStateWith fromStream
{-# INLINE makeState #-}

makeStateWith :: (ParseInput s) => (i -> s) -> i -> More -> ParseState e s
makeStateWith f inp m = PSt { input       = f inp
                            , add         = mempty
                            , more        = m
                            , errLog      = mempty
                            , isCommitted = False
                            }
{-# INLINE makeStateWith #-}

-- | Run the parser on the provided input, providing the raw 'Result'
--   value.
parseInput :: (ParseInput s) => Parser e s a -> Stream s -> Result e s a
parseInput = parseInputWith fromStream
{-# INLINE parseInput #-}

parseInputWith :: (ParseInput s) => (i -> s) -> Parser e s a -> i -> Result e s a
parseInputWith f p inp = runP p (makeStateWith f inp Incomplete) failure successful
{-# INLINE parseInputWith #-}

-- | Run the specified parser using the input provided by the first
--   monadic action.  If additional input is required the second
--   monadic action is used (if none is available, use @return
--   mempty@).
--
--   Two different monadic actions are required in case a different
--   action is needed for additional input compared to the original
--   input source.
parseAndFeed :: (ParseInput s, Monad m) => Parser e s a
                -> m (Stream s) -> m (Stream s) -> m (EitherResult e s a, Stream s)
parseAndFeed = parseAndFeedWith fromStream

-- Unless we want to change the type of Partial - which probably isn't
-- a good idea: feeders of additional input should only provide the
-- raw Stream without any wrappers - then the additional provider must
-- still produce a Stream value.
parseAndFeedWith :: (ParseInput s, Monad m) => (i -> s) -> Parser e s a
                    -> m i -> m (Stream s) -> m (EitherResult e s a, Stream s)
parseAndFeedWith f p minp madd = minp >>= go . parseInputWith f p
  where
    go (Partial _pl prt) = madd >>= go . prt
    go res               = return (resultToEither res)

-- | Run a parser.
runParser :: (ParseInput s) => Parser e s a -> Stream s
             -> (EitherResult e s a, Stream s)
runParser p inp = resultToEither (runP p (makeState inp Complete) failure successful)
{-# INLINE runParser #-}

-- | Run a parser, assuming it succeeds.  If the parser fails, use
--   'error' to display the message.
runParser' :: (PrettyValue e, ParseInput s) => Parser e s a -> Stream s -> a
runParser' p inp = case fst $ runParser p inp of
                     Right a -> a
                     Left pl -> error ('\n' : prettyLog pl)

-- -----------------------------------------------------------------------------
-- Error Handling

mergedLog :: ParseState e s -> ParseLog e s
mergedLog = mergeStack . errLog
{-# INLINE mergedLog #-}

-- A variant of 'runP' where a new ParseLog is created solely for the
-- provided parser.
--
-- If dealing specially with commitment, then the Failure case needs
-- to make sure to put the values back on the stack.
runPStacked :: Parser e s a
               -> ( forall r.
                       ParseState e s
                    -> (ParseLog e s -> Failure e s   r)
                    -> (ParseLog e s -> Success e s a r)
                    -> Result e s r
                  )
runPStacked p pSt pfl psc =
  let withLog pf pSt' = uncurry pf (getLog pSt')
  in runP p (pSt { errLog = newTop (errLog pSt) }) (withLog pfl) (withLog psc)
{-# INLINE runPStacked #-}

setErrLog :: Stack (ParseLog e s) -> ParseState e s -> ParseState e s
setErrLog el pSt = pSt { errLog = el }
{-# INLINE setErrLog #-}

getLog :: ParseState e s -> (ParseLog e s, ParseState e s)
getLog pSt = second (`setErrLog` pSt) (pop $ errLog pSt)
{-# INLINE getLog #-}

-- | Fail with a specific error.  When @OverloadedStrings@ is enabled,
--   this becomes equivalent to 'fail' (at least for literal 'String's).
failWith :: ParseError e s -> Parser e s a
failWith e = P $ \ pSt fl _sc -> fl pSt e
{-# INLINE failWith #-}

-- | A convenient function to produce (reasonably) pretty stack traces
--   for parsing failures.
--
--   Note that this is useful for \"informational\" messages; if you
--   want an error to appear only if the parser fails, see
--   'addErrOnFailure' instead.
addStackTrace :: ParseError e s -> Parser e s a -> Parser e s a
addStackTrace e p = P $ \ pSt fl sc ->
  runP p (addErr e pSt) fl sc
{-# INLINE addStackTrace #-}

addErr :: ParseError e s -> ParseState e s -> ParseState e s
addErr e pSt = setErrLog (addErrMsg e (input pSt) (errLog pSt)) pSt
{-# INLINE addErr #-}

addErrMsg :: ParseError e s -> s -> Stack (ParseLog e s) -> Stack (ParseLog e s)
addErrMsg e inp = withTop (\ el -> logError el e inp)
{-# INLINE addErrMsg #-}

-- | A variant of 'addStackTrace' that only adds the error message if
--   the parser fails.
--
--   This makes a difference in cases like @addErrOnFailure e p1 *>
--   p2@: if @p1@ succeeds then the error message @e@ won't appear in
--   the parse log.  This can help avoid cluttering up the log.
addErrOnFailure :: ParseError e s -> Parser e s a -> Parser e s a
addErrOnFailure e p = P $ \ pSt fl sc ->
  let  fl' pl pSt' = fl (setErrLog (withTop (<>pl) (errLog pSt)) pSt')
  in runPStacked (addStackTrace e p) pSt fl' (const sc)
{-# INLINE addErrOnFailure #-}

-- | Name the parser, as a shorter variant of specifying a longer
--   error message.
(<?>) :: Parser e s a -> String -> Parser e s a
p <?> f = addErrOnFailure (ParserName f) p
{-# INLINE (<?>) #-}
infix 0 <?>

-- -----------------------------------------------------------------------------
-- Instances

instance Functor (Parser e s) where
  fmap = fmapP
  {-# INLINE fmap #-}

fmapP :: (a -> b) -> Parser e s a -> Parser e s b
fmapP f pa = P $ \ pSt fl sc ->
                 runP pa pSt fl $ \ pSt' a -> sc pSt' (f a)
{-# INLINE fmapP #-}

instance Applicative (Parser e s) where
  pure = returnP
  {-# INLINE pure #-}

  (<*>) = apP
  {-# INLINE (<*>) #-}

  (*>) = ignFirstP
  {-# INLINE (*>) #-}

  (<*) = discard
  {-# INLINE (<*) #-}

returnP :: a -> Parser e s a
returnP a = P $ \ pSt _fl sc -> sc pSt a
{-# INLINE returnP #-}

-- Explicit version of @pa >>= const pb@.
ignFirstP :: Parser e s a -> Parser e s b -> Parser e s b
ignFirstP pa pb = P $ \ pSt fl sc ->
                        runP pa pSt fl $ \ pSt' _a
                          -> runP pb pSt' fl sc
{-# INLINE ignFirstP #-}

discard :: Parser e s a -> Parser e s b -> Parser e s a
discard pa pb = P $ \ pSt fl sc ->
                  let sc' a pSt' b = b `seq` sc pSt' a
                      -- Ignore the provided result and use the one
                      -- you obtained earlier.
                  in runP pa pSt fl $ \ pSt' a ->
                       runP pb pSt' fl (sc' a)
{-# INLINE discard #-}

apP :: Parser e s (a -> b) -> Parser e s a -> Parser e s b
apP pf pa = P $ \ pSt fl sc ->
                  runP pf pSt fl $ \ pSt' f ->
                    runP pa pSt' fl $ \ pSt'' a ->
                      sc pSt'' (f a)
{-# INLINE apP #-}

-- | It is /highly/ recommended that you call 'commit' on any parser
--   after using 'many' or 'some' successfully as you are unable to
--   backtrack anyway (note that 'many' will /always/ succeed).
instance (ParseInput s) => Alternative (Parser e s) where
  empty = failP "empty"
  {-# INLINE empty #-}

  (<|>) = onFailW
  {-# INLINE (<|>) #-}

  -- We can't just wrap commitment once at the top level: if the
  -- provided parser commits, then we'll never be able to backtrack to
  -- @pure []@ when we can't parse any more.

  many v = many_v <?> "many"
    where
      many_v = some_v <|> pure []
      some_v = liftA2 (:) v many_v
  {-# INLINE many #-}

  some v = some_v <?> "some"
    where
      many_v = some_v <|> pure []
      some_v = liftA2 (:) v many_v
  {-# INLINE some #-}

-- Variant of onFail that takes care of pre-commitment.
onFailW :: (ParseInput s) => Parser e s a -> Parser e s a -> Parser e s a
onFailW p1 p2 = wrapCommitment (p1 `onFail` p2)
{-# INLINE onFailW #-}

onFail :: (ParseInput s) => Parser e s a -> Parser e s a -> Parser e s a
onFail p1 p2 = onFailWith p1Fl p1Sc p1
  where
    p1Fl el = addErrOnFailure (Backtrack el) p2
    p1Sc el = const (<>el)
{-# INLINE onFail #-}

onFailWith :: (ParseInput s)
              => (ParsingErrors e s -> Parser e s a)
                 -- ^ Construct the parser for the failure case
              -> (ParseLog e s -> s -> ParseLog e s -> ParseLog e s)
                 -- ^ For successful parses and committed failures,
                 --   how to combine the sub-log with the existing
                 --   log.  The provided input value is from the
                 --   initial run of the parser in case overall errors
                 --   are to be added.
              -> Parser e s a -> Parser e s a
onFailWith fp fs p = P $ \ pSt fl sc ->
  let toFL pl' pSt' e
           | isCommitted pSt' = failure (rebaseState pl' pSt') e
           | otherwise        = let el = createFinalLog pl' e (input pSt)
                                in mergeIncremental pSt pSt' $
                                     \ pSt'' -> runP (fp el) pSt'' fl sc
        -- If we fail - and aren't committed - create the specified
        -- parser and run it instead.  Also ensure that if p1
        -- requested and obtained additional input that we use it as
        -- well (in case we backtrack further up).

      toSc pl' pSt' = sc (rebaseState pl' pSt')

      rebaseState pl' pSt' = pSt' { add    = withAdd (add pSt')
                                  -- Note: use input of pSt for fs, not pSt'!
                                  , errLog = withTop (fs pl' (input pSt)) (errLog pSt')
                                  }
        where
          -- If committed, we ignore any additional input that came previously.
          withAdd
            | isCommitted pSt' = id
            | otherwise        = (add pSt <>)

  in ignoreAdditional pSt $ \ pSt' -> runPStacked p pSt' toFL toSc
     -- We want to be able to differentiate the additional values that
     -- we already have vs any we might get from running @p@.
{-# INLINE onFailWith #-}

-- Used when you temporarily want to assume that a parser isn't
-- committed, usually because you want to see if a component makes it
-- committed.
wrapCommitment :: Parser e s a -> Parser e s a
wrapCommitment p = P $ \ pSt fl sc ->
  let origCommit = isCommitted pSt
      fl' pSt' = fl (pSt' { isCommitted = origCommit || isCommitted pSt' })
      sc' pSt' = sc (pSt' { isCommitted = origCommit || isCommitted pSt' })
  in runP p (pSt { isCommitted = False }) fl' sc'
{-# INLINE wrapCommitment #-}

instance Monad (Parser e s) where
  return = returnP
  {-# INLINE return #-}

  (>>=) = bindP
  {-# INLINE (>>=) #-}

  (>>) = ignFirstP
  {-# INLINE (>>) #-}

  fail = failP
  {-# INLINE fail #-}

failP :: String -> Parser e s a
failP = failWith . Message
{-# INLINE failP #-}

bindP ::  Parser e s a -> (a -> Parser e s b) -> Parser e s b
bindP p f = P $ \ pSt fl sc -> runP p pSt fl $
                 -- Get the new parser and run it.
                  \ pSt' a -> runP (f a) pSt' fl sc
{-# INLINE bindP #-}

instance (ParseInput s) => MonadPlus (Parser e s) where
  mzero = failP "mzero"
  {-# INLINE mzero #-}

  mplus = onFail
  {-# INLINE mplus #-}

-- | This uses the 'stream' parser.
instance (ParseInput s, Eq (Stream s), IsString (Stream s))
         => IsString (Parser e s ()) where
  fromString = stream . fromString

-- | Match a specified sub-stream.
stream :: (ParseInput s, Eq (Stream s)) => Stream s -> Parser e s ()
stream str = do
  let n = LL.length str
  inp <- getAtLeast n
  let (!pre,suf) = getStreamLength n inp
  if str == pre
     then put suf
     else failWith (ExpectedButFoundStream str pre)

-- -----------------------------------------------------------------------------
-- Commitment

-- Note that we only keep the additional input around in case we
-- backtrack and need to restore what was added when going to a
-- different branch.  As such, since commit doesn't allow
-- backtracking, there's no point keeping the additional input around
-- as it won't get used!.
commitState :: (ParseInput s) => ParseState e s -> ParseState e s
commitState pSt = pSt { add         = LL.empty
                      , isCommitted = True
                      }
{-# INLINE commitState #-}

-- -----------------------------------------------------------------------------
-- Some basic parsers

get :: Parser e s s
get = P $ \ pSt _fl sc -> sc pSt (input pSt)
{-# INLINE get #-}

put :: s -> Parser e s ()
put s = P $ \ pSt _fl sc -> sc (pSt { input = s }) ()
{-# INLINE put #-}

-- -----------------------------------------------------------------------------
-- Incremental support

-- @mergeIncremental inc1 inc2@ is used when @inc2@ originally started
-- as having the same 'received' input as @inc1@, but may have since
-- received additional input.
mergeIncremental :: (ParseInput s) => ParseState e s -> ParseState e s
                    -> (ParseState e s -> r) -> r
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
ignoreAdditional :: (ParseInput s) => ParseState e s -> (ParseState e s -> r) -> r
ignoreAdditional pSt f = f ( pSt { add = mempty } )
{-# INLINE ignoreAdditional #-}

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

-- | A variant of 'needMoreInput' that always succeeds, even if no
--   more input is available.
tryGetMoreInput :: (ParseInput s) => Parser e s ()
tryGetMoreInput = P $ \ pSt _fl sc ->
  if more pSt == Complete
     then sc pSt ()
     else requestInput pSt (`sc` ()) (`sc` ())

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

-- -----------------------------------------------------------------------------
-- Utility functions

-- | Is the current state of the input empty?
isEmpty :: (ParseInput s) => s -> Bool
isEmpty = LL.null . getStream
{-# INLINE isEmpty #-}
