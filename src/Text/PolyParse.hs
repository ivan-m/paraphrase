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
import Data.Monoid

-- Sources
{-
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
-}

-- -----------------------------------------------------------------------------

class (Monoid s) => ParseValue s where

  type Token s

  -- | Attempt to extract the first token.
  uncons :: s -> Maybe (Token s, s)

  -- | Is the input empty?
  isEmpty :: s -> Bool

  -- | Split the stream where the predicate is no longer satisfied.
  breakWhen :: (Token s -> Bool) -> s -> (s,s)

instance ParseValue [a] where
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
type Success s a r = s -> AdjErr -> a      -> Result s r

noAdj :: AdjErr
noAdj = id

failure :: Failure s r
failure s adjE e = Failure s (adjE e)

successful :: Success s a a
successful s _ = Success s

runParser :: Parser s a -> s -> (Either String a, s)
runParser p inp = resultToEither $ runP p inp noAdj failure successful

runParser' :: Parser s a -> s -> a
runParser' p inp = case fst $ runParser p inp of
                     Right a  -> a
                     Left err -> error err


-- -----------------------------------------------------------------------------
-- Instances

instance Functor (Parser s) where
  fmap = fmapP
  {-# INLINE fmap #-}

fmapP :: (a -> b) -> Parser s a -> Parser s b
fmapP f pa = P $ \ inp adjE fl sc ->
                 runP pa inp adjE fl $ \ inp' adjE' a ->
                                          sc inp' adjE' (f a)

instance Applicative (Parser s) where
  pure = returnP
  {-# INLINE pure #-}

  (<*>) = apP
  {-# INLINE (<*>) #-}

  (*>) = (>>)
  {-# INLINE (*>) #-}

  p1 <* p2 = p1 >>= \ a -> p2 >> return a
  {-# INLINE (<*) #-}

instance Alternative (Parser s) where
  empty = failP "empty"
  {-# INLINE empty #-}

  (<|>) = onFail
  {-# INLINE (<|>) #-}

instance Monad (Parser s) where
  return = returnP
  {-# INLINE return #-}

  fail = failP
  {-# INLINE fail #-}

  (>>=) = bindP
  {-# INLINE (>>=) #-}

returnP :: a -> Parser s a
returnP a = P $ \ inp adjE _fl sc -> sc inp adjE a
{-# INLINE returnP #-}

apP :: Parser s (a -> b) -> Parser s a -> Parser s b
apP pf pa = do
    f <- pf
    a <- pa
    return (f a)
{-# INLINE apP #-}

failP :: String -> Parser s a
failP e = P $ \ inp adjE fl _sc -> fl inp adjE e
{-# INLINE failP #-}

bindP ::  Parser s a -> (a -> Parser s b) -> Parser s b
bindP p f = P $ \ inp adjE fl sc -> runP p inp adjE fl $
                  \inp' adjE' a -> runP (f a) inp' adjE' fl sc
{-# INLINE bindP #-}

onFail :: Parser s a -> Parser s a -> Parser s a
onFail p1 p2 = P $ \ inp adjE fl sc ->
               let fl' inp' _adjE' _e = runP p2 inp' adjE fl sc
                   sc' inp' adjE'    = sc inp' (adjE . adjE')
               in runP p1 inp adjE fl' sc'
{-# INLINE onFail #-}

-- -----------------------------------------------------------------------------
-- Commitment

commit :: Parser s a -> Parser s a
commit p = P $ \ inp adjE _fl sc -> runP p inp adjE failure sc
{-# INLINE commit #-}

failBad :: String -> Parser s a
failBad = commit . fail

-- adjustErr and adjustErrBad need Failure to take a list of Strings I
-- think.

next :: (ParseValue s) => Parser s (Token s)
next = P $ \ inp adjE fl sc ->
             case uncons inp of
               Nothing -> fl inp adjE "Ran out of input (EOF)"
               Just (t,inp') -> sc inp' adjE t
{-# INLINE next #-}

eof :: (ParseValue s) => Parser s ()
eof = P $ \ inp adjE fl sc ->
            if isEmpty inp
               then sc inp adjE ()
               else fl inp adjE "Expected end of input (EOF)"
{-# INLINE eof #-}

satisfy :: (ParseValue s) => (Token s -> Bool) -> Parser s (Token s)
satisfy f = do
  x <- next
  if f x
     then return x
     else fail "Token did not satisfy predicate"
{-# INLINE satisfy #-}

manySatisfy :: (ParseValue s) => (Token s -> Bool) -> Parser s s
manySatisfy f = P $ \ inp adjE _fl sc ->
                  let (pre,suf) = breakWhen f inp
                  in sc suf adjE pre
{-# INLINE manySatisfy #-}

many1Satisfy :: (ParseValue s) => (Token s -> Bool) -> Parser s s
many1Satisfy f = P $ \ inp adjE fl sc ->
                   let (pre,suf) = breakWhen f inp
                   in if isEmpty pre
                         then fl inp adjE "many1Satisfy: failed"
                         else sc suf adjE pre
{-# INLINE many1Satisfy #-}

reparse :: (ParseValue s) => s -> Parser s ()
reparse s = P $ \ inp adjE _fl sc -> sc (s <> inp) adjE ()
{-# INLINE reparse #-}

adjustErr :: Parser s a -> (String -> String) -> Parser s a
adjustErr p f = P $ \ inp adjE fl sc ->
                       runP p inp (adjE . f) fl sc

addStackTrace :: String -> Parser s a -> Parser s a
addStackTrace msg = (`adjustErr`((msg ++ "\n\t")++))

-- Check adjE usage for onFail: the new failure one shouldn't use the
-- adjE from the success
