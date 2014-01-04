{-# LANGUAGE RankNTypes, TypeFamilies #-}
{- |
   Module      : Text.PolyParse
   Description : Experimental polyparse reimplementation
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This is an experiment in creating a modern combinator-parsing
   library based upon the @polyparse@ api.

 -}
module Text.PolyParse where

import Control.Applicative
import Control.Monad       (MonadPlus (..))
import Data.Monoid

-- Sources
{-
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
-}

-- -----------------------------------------------------------------------------

class (Monoid s) => ParseInput s where

  type Token s

  -- | Attempt to extract the first token.
  uncons :: s -> Maybe (Token s, s)

  -- | Is the input empty?
  isEmpty :: s -> Bool

  -- | Split the stream where the predicate is no longer satisfied.
  breakWhen :: (Token s -> Bool) -> s -> (s,s)

instance ParseInput [a] where
  type Token [a] = a

  uncons []     = Nothing
  uncons (a:as) = Just (a,as)

  isEmpty = null

  breakWhen = span

-- -----------------------------------------------------------------------------

data Result s a = Success s a
                | Failure s String
                deriving (Eq, Ord, Show, Read)

instance Functor (Result s) where
  fmap f (Success s a) = Success s (f a)
  fmap _ (Failure s e) = Failure s e

resultToEither :: Result s a -> (Either String a, s)
resultToEither (Success s a) = (Right a, s)
resultToEither (Failure s e) = (Left e, s)

-- -----------------------------------------------------------------------------

newtype Parser s a = P {
  runP :: forall r. s
       -> AdjErr
       -> Failure s   r
       -> Success s a r
       -> Result  s   r
  }

type ErrMsg = String
type AdjErr = ErrMsg -> ErrMsg
type Failure s   r = s -> AdjErr -> ErrMsg -> Result s r
type Success s a r = s           -> a      -> Result s r

noAdj :: AdjErr
noAdj = id

failure :: Failure s r
failure s adjE e = Failure s (adjE e)

successful :: Success s a a
successful = Success

runParser :: Parser s a -> s -> (Either String a, s)
runParser p inp = resultToEither $ runP p inp noAdj failure successful

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
                  \inp' a -> runP (f a) inp' adjE fl sc
{-# INLINE bindP #-}

onFail :: Parser s a -> Parser s a -> Parser s a
onFail p1 p2 = P $ \ inp adjE fl sc ->
               let fl' _inp' _adjE' _e = runP p2 inp adjE fl sc
                   sc' inp'           = sc inp'
               in runP p1 inp adjE fl' sc'
{-# INLINE onFail #-}

-- -----------------------------------------------------------------------------
-- Commitment

commit :: Parser s a -> Parser s a
commit p = P $ \ inp adjE _fl sc -> runP p inp adjE failure sc
{-# INLINE commit #-}

failBad :: String -> Parser s a
failBad = commit . fail
{-# INLINE failBad #-}

-- -----------------------------------------------------------------------------
-- Combinators

next :: (ParseInput s) => Parser s (Token s)
next = P $ \ inp adjE fl sc -> maybe (fl inp adjE "Ran out of input (EOF)")
                                     (uncurry $ flip sc)
                                     (uncons inp)
{-# INLINE next #-}

eof :: (ParseInput s) => Parser s ()
eof = P $ \ inp adjE fl sc ->
            if isEmpty inp
               then sc inp ()
               else fl inp adjE "Expected end of input (EOF)"
{-# INLINE eof #-}

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

oneOf :: [Parser s a] -> Parser s a
oneOf = foldr (<|>) (fail "Failed to parse any of the possible choices")
{-# INLINE oneOf #-}

manySatisfy :: (ParseInput s) => (Token s -> Bool) -> Parser s s
manySatisfy f = P $ \ inp _adjE _fl sc ->
                  let (pre,suf) = breakWhen f inp
                  in sc suf pre
{-# INLINE manySatisfy #-}

someSatisfy :: (ParseInput s) => (Token s -> Bool) -> Parser s s
someSatisfy f = P $ \ inp adjE fl sc ->
                    let (pre,suf) = breakWhen f inp
                    in if isEmpty pre
                          then fl inp adjE "someSatisfy: failed"
                          else sc suf pre
{-# INLINE someSatisfy #-}

reparse :: (ParseInput s) => s -> Parser s ()
reparse s = P $ \ inp _adjE _fl sc -> sc (s <> inp) ()
{-# INLINE reparse #-}

-- -----------------------------------------------------------------------------
-- Separating/discarding combinators

sepBy :: Parser s a -> Parser s sep -> Parser s [a]
sepBy p sep = sepBy1 p sep <|> pure []
{-# INLINE sepBy #-}

sepBy1 :: Parser s a -> Parser s sep -> Parser s [a]
sepBy1 p sep = addStackTrace "When looking for a non-empty sequence with separators"
               $ liftA2 (:) p (many (sep *> p))
{-# INLINE sepBy1 #-}

bracket :: Parser s bra -> Parser s ket -> Parser s a -> Parser s a
bracket open close p = open' *> p <* close'
  where
    open'  = addStackTrace "Missing opening bracket:" open
    close' = addStackTrace "Missing closing bracket:" close
{-# INLINE bracket #-}

bracketSep :: Parser s bra -> Parser s sep -> Parser s ket
              -> Parser s a -> Parser s [a]
bracketSep open sep close p = bracket open (commit close) (sepBy p sep)
{-# INLINE bracketSep #-}

manyFinally :: Parser s a -> Parser s z -> Parser s [a]
manyFinally p t = (many p <* t)
                  <|> oneOf' [ ("sequence terminator",t *> pure [])
                             , ("item in a sequence", p *> pure [])
                             ]
{-# INLINE manyFinally #-}

manyFinally' :: Parser s a -> Parser s z -> Parser s [a]
manyFinally' p t = go
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

exactly :: Int -> Parser s a -> Parser s [a]
exactly n p = mapM toP $ enumFromThenTo n (n-1) 1
  where
    toP = (`addStackTrace` p) . msg
    msg c = "Expecting precisely " ++ show c ++ " item(s)."
{-# INLINE exactly #-}

upto :: Int -> Parser s a -> Parser s [a]
upto n p = foldr go (pure []) $ replicate n p
  where
    -- Not taking argument as we _know_ it's `p'...
    go _ lst = liftA2 (:) p lst <|> pure []
{-# INLINE upto #-}

-- -----------------------------------------------------------------------------
-- Manipulating error messages

failMessage :: String -> Parser s a -> Parser s a
failMessage e = (`onFail` fail e)
{-# INLINE failMessage #-}

adjustErr :: Parser s a -> (String -> String) -> Parser s a
adjustErr p f = P $ \ inp adjE fl sc ->
                       runP p inp (adjE . f) fl sc
{-# INLINE adjustErr #-}

adjustErrBad :: Parser s a -> (String -> String) -> Parser s a
adjustErrBad = adjustErr . commit
{-# INLINE adjustErrBad #-}

addStackTrace :: String -> Parser s a -> Parser s a
addStackTrace msg = (`adjustErr`((msg'++) . (('\n':line:marker)++)))
  where
    msg' = case lines msg of
             [_]      -> msg
             (ln:lns) -> unlines $ ln : map ind lns

    ind = ('|':) . indentLine markerLength

    line = '|'
    marker = "-> "
    markerLength = length marker
{-# INLINE addStackTrace #-}

indent :: Int -> String -> String
indent n = unlines . map (indentLine n) . lines
{-# INLINE indent #-}

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
          fl' _inp _adjE e = runP (go' e) inp adjE fl sc
      in runP p inp adjE fl' sc

    showErr (nm,e) = "* " ++ nm ++ ":\n" ++ indent 4 e
