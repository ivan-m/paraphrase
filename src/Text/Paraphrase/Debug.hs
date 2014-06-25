{- |
   Module      : Text.Paraphrase.Debug
   Description : Debugging utils to test parsers
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   Extra utility functions to obtain internal state of the parser.

 -}
module Text.Paraphrase.Debug where

import Text.Paraphrase.Errors (ParseError (LogRequested), ParsingErrors,
                               createFinalLog)
import Text.Paraphrase.Types

-- -----------------------------------------------------------------------------

isParserCommitted :: Parser e s Bool
isParserCommitted = P $ \ pSt _fl sc -> sc pSt (isCommitted pSt)

getCurrentLog :: Parser e s (ParsingErrors e s)
getCurrentLog = P $ \ pSt _fl sc ->
  sc pSt (createFinalLog (mergedLog pSt) LogRequested (input pSt))
