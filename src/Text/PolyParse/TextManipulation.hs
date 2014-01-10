{- |
   Module      : Text.PolyParse.TextManipulation
   Description : Manipulating strings
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Common functions used to manipulate error messages, etc.

 -}
module Text.PolyParse.TextManipulation where

-- -----------------------------------------------------------------------------

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

-- | A convenience function for use with 'adjustErr' useful for
--   formatting error messages; indents /all/ lines by a fixed amount.
indent :: Int -> String -> String
indent n = unlines . map (indentLine n) . lines
{-# INLINE indent #-}

-- | As with 'indent' but assumes the error message is a single line.
indentLine :: Int -> String -> String
indentLine n = (replicate n ' ' ++)
{-# INLINE indentLine #-}

-- | Map a function over all but the first line of text.  Useful when
--   you want all subsequent lines indented.
allButFirstLine :: (String -> String) -> String -> String
allButFirstLine f msg = case lines msg of
                          [_]      -> msg
                          (ln:lns) -> unlines $ ln : map f lns
                          _        -> msg
{-# INLINE allButFirstLine #-}
