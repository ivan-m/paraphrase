{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |
   Module      : Text.Paraphrase.Stack
   Description : A monoidal stack implementation
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   A stack for monoidal values, such that popping an empty stack will
   return 'mempty' rather than an error.

 -}
module Text.Paraphrase.Stack
  ( Stack
  , push
  , newTop
  , pop
  , mergeStack
  , withTop
  ) where

import Control.Arrow (first)
import Data.IsNull
import Data.Monoid

-- -----------------------------------------------------------------------------

newtype Stack a = Stack { unStack :: [a] }
                  deriving (Eq, Ord, Show, Read, Monoid, IsNull, Functor)

push :: a -> Stack a -> Stack a
push a = Stack . (a:) . unStack

newTop :: (Monoid a) => Stack a -> Stack a
newTop = push mempty

pop :: (Monoid a) => Stack a -> (a, Stack a)
pop st = case unStack st of
           []   -> (mempty, st)
           a:as -> (a, Stack as)

-- Newest values on top that should go on end.  Should probably
-- replace with a custom fold rather than using reverse directly.
mergeStack :: (Monoid a) => Stack a -> a
mergeStack = mconcat . reverse . unStack

withTop :: (Monoid a) => (a -> a) -> Stack a -> Stack a
withTop f = uncurry push . first f . pop
