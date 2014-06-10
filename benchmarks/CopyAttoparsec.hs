{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, OverloadedStrings,
             Rank2Types, RecordWildCards #-}
{- |
   Module      : CopyAttoparsec
   Description : Copying attoparsec for fun and profit
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module CopyAttoparsec where

import Control.Applicative (Alternative (..), Applicative (..), (<$>))
import Control.DeepSeq     (NFData (rnf))
import Control.Monad       (MonadPlus (..))
import Data.Monoid         (Monoid (..), (<>))
import Prelude             hiding (getChar, take, takeWhile)

import Data.Word (Word8)


import qualified Data.ByteString.Char8  as B
import qualified Data.ByteString.Unsafe as B

-- -----------------------------------------------------------------------------

-- | The result of a parse.  This is parameterised over the type @t@
-- of string that was processed.
--
-- This type is an instance of 'Functor', where 'fmap' transforms the
-- value in a 'Done' result.
data IResult' t r = Fail t [String] String
                 -- ^ The parse failed.  The 't' parameter is the
                 -- input that had not yet been consumed when the
                 -- failure occurred.  The @[@'String'@]@ is a list of
                 -- contexts in which the error occurred.  The
                 -- 'String' is the message describing the error, if
                 -- any.
                 | Partial (t -> IResult' t r)
                 -- ^ Supply this continuation with more input so that
                 -- the parser can resume.  To indicate that no more
                 -- input is available, use an empty string.
                 | Done t r
                 -- ^ The parse succeeded.  The 't' parameter is the
                 -- input that had not yet been consumed (if any) when
                 -- the parse succeeded.

instance (Show t, Show r) => Show (IResult' t r) where
    show (Fail t stk msg) =
        "Fail " ++ show t ++ " " ++ show stk ++ " " ++ show msg
    show (Partial _)      = "Partial _"
    show (Done t r)       = "Done " ++ show t ++ " " ++ show r

instance (NFData t, NFData r) => NFData (IResult' t r) where
    rnf (Fail t stk msg) = rnf t `seq` rnf stk `seq` rnf msg
    rnf (Partial _)  = ()
    rnf (Done t r)   = rnf t `seq` rnf r
    {-# INLINE rnf #-}

fmapR :: (a -> b) -> IResult' t a -> IResult' t b
fmapR _ (Fail t stk msg) = Fail t stk msg
fmapR f (Partial k)       = Partial (fmapR f . k)
fmapR f (Done t r)       = Done t (f r)

instance Functor (IResult' t) where
    fmap = fmapR
    {-# INLINE fmap #-}

newtype Input' t = I {unI :: t} deriving (Monoid)
newtype Added' t = A {unA :: t} deriving (Monoid)

-- | The core parser type.  This is parameterised over the type @t@ of
-- string being processed.
--
-- This type is an instance of the following classes:
--
-- * 'Monad', where 'fail' throws an exception (i.e. fails) with an
--   error message.
--
-- * 'Functor' and 'Applicative', which follow the usual definitions.
--
-- * 'MonadPlus', where 'mzero' fails (with no error message) and
--   'mplus' executes the right-hand parser if the left-hand one
--   fails.  When the parser on the right executes, the input is reset
--   to the same state as the parser on the left started with. (In
--   other words, Attoparsec is a backtracking parser that supports
--   arbitrary lookahead.)
--
-- * 'Alternative', which follows 'MonadPlus'.
newtype Parser' t a = Parser' {
      runParser' :: forall r. Input' t -> Added' t -> More
                -> Failure' t   r
                -> Success' t a r
                -> IResult' t r
    }

type Failure' t   r = Input' t -> Added' t -> More -> [String] -> String
                   -> IResult' t r
type Success' t a r = Input' t -> Added' t -> More -> a -> IResult' t r

-- | Have we read all available input?
data More = Complete | Incomplete
            deriving (Eq, Show)

instance Monoid More where
    mappend c@Complete _ = c
    mappend _ m          = m
    mempty               = Incomplete

addS :: (Monoid t) =>
        Input' t -> Added' t -> More
     -> Input' t -> Added' t -> More
     -> (Input' t -> Added' t -> More -> r) -> r
addS i0 a0 m0 _i1 a1 m1 f =
    let !i = i0 <> I (unA a1)
        a  = a0 <> a1
        !m = m0 <> m1
    in f i a m
{-# INLINE addS #-}

bindP :: Parser' t a -> (a -> Parser' t b) -> Parser' t b
bindP m g =
    Parser' $ \i0 a0 m0 kf ks -> runParser' m i0 a0 m0 kf $
                                \i1 a1 m1 a -> runParser' (g a) i1 a1 m1 kf ks
{-# INLINE bindP #-}

returnP :: a -> Parser' t a
returnP a = Parser' (\i0 a0 m0 _kf ks -> ks i0 a0 m0 a)
{-# INLINE returnP #-}

instance Monad (Parser' t) where
    return = returnP
    (>>=)  = bindP
    fail   = failDesc

noAdds :: (Monoid t) =>
          Input' t -> Added' t -> More
       -> (Input' t -> Added' t -> More -> r) -> r
noAdds i0 _a0 m0 f = f i0 mempty m0
{-# INLINE noAdds #-}

plus :: (Monoid t) => Parser' t a -> Parser' t a -> Parser' t a
plus a b = Parser' $ \i0 a0 m0 kf ks ->
           let kf' i1 a1 m1 _ _ = addS i0 a0 m0 i1 a1 m1 $
                                  \ i2 a2 m2 -> runParser' b i2 a2 m2 kf ks
               ks' i1 a1 m1 = ks i1 (a0 <> a1) m1
           in  noAdds i0 a0 m0 $ \i2 a2 m2 -> runParser' a i2 a2 m2 kf' ks'
{-# INLINE plus #-}

instance (Monoid t) => MonadPlus (Parser' t) where
    mzero = failDesc "mzero"
    {-# INLINE mzero #-}
    mplus = plus

fmapP :: (a -> b) -> Parser' t a -> Parser' t b
fmapP p m = Parser' $ \i0 a0 m0 f k ->
            runParser' m i0 a0 m0 f $ \i1 a1 s1 a -> k i1 a1 s1 (p a)
{-# INLINE fmapP #-}

instance Functor (Parser' t) where
    fmap = fmapP
    {-# INLINE fmap #-}

apP :: Parser' t (a -> b) -> Parser' t a -> Parser' t b
apP d e = do
  b <- d
  a <- e
  return (b a)
{-# INLINE apP #-}

instance Applicative (Parser' t) where
    pure   = returnP
    {-# INLINE pure #-}
    (<*>)  = apP
    {-# INLINE (<*>) #-}

#if MIN_VERSION_base(4,2,0)
    -- These definitions are equal to the defaults, but this
    -- way the optimizer doesn't have to work so hard to figure
    -- that out.
    (*>)   = (>>)
    {-# INLINE (*>) #-}
    x <* y = x >>= \a -> y >> return a
    {-# INLINE (<*) #-}
#endif

instance (Monoid t) => Monoid (Parser' t a) where
    mempty  = failDesc "mempty"
    {-# INLINE mempty #-}
    mappend = plus
    {-# INLINE mappend #-}

instance (Monoid t) => Alternative (Parser' t) where
    empty = failDesc "empty"
    {-# INLINE empty #-}

    (<|>) = plus
    {-# INLINE (<|>) #-}

#if MIN_VERSION_base(4,2,0)
    many v = many_v
        where many_v = some_v <|> pure []
              some_v = (:) <$> v <*> many_v
    {-# INLINE many #-}

    some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
    {-# INLINE some #-}
#endif

failDesc :: String -> Parser' t a
failDesc err = Parser' (\i0 a0 m0 kf _ks -> kf i0 a0 m0 [] msg)
    where msg = "Failed reading: " ++ err
{-# INLINE failDesc #-}

-- -----------------------------------------------------------------------------

type Parser = Parser' B.ByteString
type Result = IResult' B.ByteString
type Input = Input' B.ByteString
type Added = Added' B.ByteString
type Failure r = Failure' B.ByteString r
type Success a r = Success' B.ByteString a r

ensure' :: Int -> Input -> Added -> More -> Failure r -> Success B.ByteString r
        -> IResult' B.ByteString r
ensure' !n0 i0 a0 m0 kf0 ks0 =
    runParser' (demandInput >> go n0) i0 a0 m0 kf0 ks0
  where
    go !n = Parser' $ \i a m kf ks ->
        if B.length (unI i) >= n
        then ks i a m (unI i)
        else runParser' (demandInput >> go n) i a m kf ks

-- | If at least @n@ bytes of input are available, return the current
-- input, otherwise fail.
ensure :: Int -> Parser B.ByteString
ensure !n = Parser' $ \i0 a0 m0 kf ks ->
    if B.length (unI i0) >= n
    then ks i0 a0 m0 (unI i0)
    -- The uncommon case is kept out-of-line to reduce code size:
    else ensure' n i0 a0 m0 kf ks
-- Non-recursive so the bounds check can be inlined:
{-# INLINE ensure #-}

-- | Ask for input.  If we receive any, pass it to a success
-- continuation, otherwise to a failure continuation.
prompt :: Input -> Added -> More
       -> (Input -> Added -> More -> Result r)
       -> (Input -> Added -> More -> Result r)
       -> Result r
prompt i0 a0 _m0 kf ks = Partial $ \s ->
    if B.null s
    then kf i0 a0 Complete
    else ks (i0 <> I s) (a0 <> A s) Incomplete

-- | Immediately demand more input via a 'Partial' continuation
-- result.
demandInput :: Parser ()
demandInput = Parser' $ \i0 a0 m0 kf ks ->
    if m0 == Complete
    then kf i0 a0 m0 ["demandInput"] "not enough bytes"
    else let kf' i a m = kf i a m ["demandInput"] "not enough bytes"
             ks' i a m = ks i a m ()
         in prompt i0 a0 m0 kf' ks'

-- | This parser always succeeds.  It returns 'True' if any input is
-- available either immediately or on demand, and 'False' if the end
-- of all input has been reached.
wantInput :: Parser Bool
wantInput = Parser' $ \i0 a0 m0 _kf ks ->
  case () of
    _ | not (B.null (unI i0)) -> ks i0 a0 m0 True
      | m0 == Complete  -> ks i0 a0 m0 False
      | otherwise       -> let kf' i a m = ks i a m False
                               ks' i a m = ks i a m True
                           in prompt i0 a0 m0 kf' ks'

get :: Parser B.ByteString
get  = Parser' $ \i0 a0 m0 _kf ks -> ks i0 a0 m0 (unI i0)

put :: B.ByteString -> Parser ()
put s = Parser' $ \_i0 a0 m0 _kf ks -> ks (I s) a0 m0 ()

-- | Attempt a parse, and if it fails, rewind the input so that no
-- input appears to have been consumed.
--
-- This combinator is provided for compatibility with Parsec.
-- Attoparsec parsers always backtrack on failure.
try :: Parser a -> Parser a
try p = p
{-# INLINE try #-}

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy p = do
  s <- ensure 1
  let !w = B.unsafeHead s
  if p w
    then put (B.unsafeTail s) >> return w
    else fail "satisfy"
{-# INLINE satisfy #-}

-- | The parser @skip p@ succeeds for any byte for which the predicate
-- @p@ returns 'True'.
--
-- >skipDigit = skip isDigit
-- >    where isDigit w = w >= 48 && w <= 57
skip :: (Word8 -> Bool) -> Parser ()
skip p = do
  s <- ensure 1
  if p (B.unsafeHead s)
    then put (B.unsafeTail s)
    else fail "skip"

-- | The parser @satisfyWith f p@ transforms a byte, and succeeds if
-- the predicate @p@ returns 'True' on the transformed value. The
-- parser returns the transformed byte that was parsed.
satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Parser a
satisfyWith f p = do
  s <- ensure 1
  let c = f $! B.unsafeHead s
  if p c
    then let !t = B.unsafeTail s
         in put t >> return c
    else fail "satisfyWith"
{-# INLINE satisfyWith #-}

-- | Match any byte.
anyWord8 :: Parser Word8
anyWord8 = satisfy $ const True
{-# INLINE anyWord8 #-}

-- | Match a specific byte.
word8 :: Word8 -> Parser Word8
word8 c = satisfy (== c)
{-# INLINE word8 #-}

-- | Run a parser.
parse :: Parser a -> B.ByteString -> Result a
parse m s = runParser' m (I s) mempty Incomplete failK successK
{-# INLINE parse #-}

-- | Run a parser that cannot be resupplied via a 'Partial' result.
parseOnly :: Parser a -> B.ByteString -> Either String a
parseOnly m s = case runParser' m (I s) mempty Complete failK successK of
                  Fail _ _ err -> Left err
                  Done _ a     -> Right a
                  _            -> error "parseOnly: impossible error!"
{-# INLINE parseOnly #-}

-- | Terminal failure continuation.
failK :: Failure a
failK i0 _a0 _m0 stack msg = Fail (unI i0) stack msg
{-# INLINE failK #-}

-- | Terminal success continuation.
successK :: Success a a
successK i0 _a0 _m0 a = Done (unI i0) a
{-# INLINE successK #-}
