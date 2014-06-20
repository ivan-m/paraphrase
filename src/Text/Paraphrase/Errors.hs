{-# LANGUAGE FlexibleContexts, StandaloneDeriving, UndecidableInstances #-}
{- |
   Module      : Text.Paraphrase.Errors
   Description : Internal module for defining error types
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module is defined so that the actual parser code can't rely
   upon a specific implementation of the error log type.

 -}
module Text.Paraphrase.Errors
  ( ParseError (..)
  , BracketType (..)
  , TaggedError
  , parseError
  , errorLocation
  , ParseLog -- Constructor not exported!
  , logError
  , streamToDoc
  , ParsingErrors
  , createFinalLog
  , finalError
  , completeLog
  , prettyLog
  , prettyDetailedLog
  , PrettyLog (..)
  , LogDetail (..)
  ) where

import Text.Paraphrase.Inputs (ParseInput (..), TokenStream (..))
import Text.Paraphrase.Pretty

import Text.PrettyPrint.HughesPJ hiding (isEmpty, (<>))

import Control.Applicative (liftA2)
import Control.Arrow       (second)
import Control.DeepSeq     (NFData (rnf))
import Data.Function       (on)
import Data.Monoid
import Data.String         (IsString (..))

-- -----------------------------------------------------------------------------

-- | The possible errors that could arise when parsing.  Please note
--   that to an extent, \"errors\" is a bit of a misnomer, as these
--   are also used for general logging whilst parsing, though these
--   logs are only provided when an actual error arises.
data ParseError s
  = UnexpectedEndOfInput
  | NoMoreInputExpected -- ^ When more input requested after being told there isn't any more.
  | ExpectedEndOfInput  -- ^ The parser expected no more input even when more remains.
  | ExpectedButFound (Token s) (Token s) -- ^ The token that was expected/required.
  | UnexpectedToken (Token s)            -- ^ Token found that did not match what was required/expected.
  | MissingItemCount Int                 -- ^ Expected this many more items that were not found.
  | MissingBracket BracketType
  | ListWithTerminator                   -- ^ Either 'manyFinally' or 'manyFinally''.
  | Message String                       -- ^ Can be created via the @OverloadedStrings@ pragma.
  | NoParserSatisfied                    -- ^ Used with 'oneOf'.
  | PredicateNotSatisfied                -- ^ Used with combinators like 'sepBy1'.
  | ParserName String                    -- ^ Used with '<?>'.
  | Reparse (Stream s)                   -- ^ Additional input added to front.
  | Committed
  | Backtrack [TaggedError s]            -- ^ The log from the left-hand-side of '(<|>)'.
  | NamedSubLogs [(String, [TaggedError s])]
  | ChainedParser
  | SubLog [TaggedError Doc]             -- ^ The log (if any) from a chained parser.
  | AwaitingInput                        -- ^ Within a 'Partial' result type.
  | LogRequested                         -- ^ Used with "Text.Paraphrase.Debug".

-- Need to make these instances separate for GHC to be happy; hence
-- also the various language pragmas above.
deriving instance (TokenStream s, Eq   s, Eq   (Stream s), Eq   (Token s)) => Eq   (ParseError s)
deriving instance (TokenStream s, Show s, Show (Stream s), Show (Token s)) => Show (ParseError s)

-- | Orphan instance needed for 'ParseError's instance.
instance NFData Doc where
  rnf = rnf . render

instance Eq Doc where
  (==) = (==) `on` render

instance (TokenStream s, NFData s, NFData (Stream s), NFData (Token s)) => NFData (ParseError s) where
  rnf (ExpectedButFound e f) = rnf e `seq` rnf f
  rnf (UnexpectedToken t)    = rnf t
  rnf (MissingItemCount n)   = rnf n
  rnf (MissingBracket b)     = rnf b
  rnf (Message msg)          = rnf msg
  rnf (ParserName nm)        = rnf nm
  rnf (Reparse s)            = rnf s
  rnf (Backtrack bl)         = rnf bl
  rnf (NamedSubLogs spls)    = rnf spls
  rnf (SubLog pl)            = rnf pl
  rnf _                      = ()

instance IsString (ParseError s) where
 fromString = Message

data BracketType = OpenBracket | CloseBracket
                    deriving (Eq, Ord, Show, Read)

instance NFData BracketType where
  rnf OpenBracket  = ()
  rnf CloseBracket = ()

instance PrettyValue BracketType where
  prettyValue OpenBracket  = text "opening"
  prettyValue CloseBracket = text "closing"

-- | A 'ParseError' tagged with the current input at the location of
--   where it arose.
data TaggedError s = TE { parseError    :: !(ParseError s)
                          -- ^ The error that arose.
                        , errorLocation :: !s
                          -- ^ The current input when the error arose.
                        }

deriving instance (TokenStream s, Eq   s, Eq   (Stream s), Eq   (Token s)) => Eq   (TaggedError s)
deriving instance (TokenStream s, Show s, Show (Stream s), Show (Token s)) => Show (TaggedError s)

instance (TokenStream s, NFData s, NFData (Stream s), NFData (Token s)) => NFData (TaggedError s) where
  rnf (TE pe el) = rnf pe `seq` rnf el

newtype ParseLog s = PL { getLog :: [TaggedError s] -> [TaggedError s]  }

instance (TokenStream s, Eq s, Eq (Stream s), Eq (Token s)) => Eq (ParseLog s) where
  (==) = (==) `on` ($[]) . getLog

instance (TokenStream s, Show s, Show (Stream s), Show (Token s)) => Show (ParseLog s) where
  showsPrec d = showsPrec d . ($[]) . getLog

instance Monoid (ParseLog s) where
  mempty = PL id

  mappend (PL l1) (PL l2) = PL (l1 . l2)

-- | Add a @ParseError@ to the end of the log.
logError :: ParseLog s -> ParseError s -> s -> ParseLog s
logError pl e inp = pl { getLog = getLog pl . ((TE e inp):) }

createFinalLog :: ParseLog s -> ParseError s -> s -> ParsingErrors s
createFinalLog pl e inp = PEs pl (TE e inp)

-- | The log of errors from parsing.
data ParsingErrors s = PEs { errorLog   :: ParseLog s
                           -- | The error which caused the parsing to
                           -- fail.
                           , finalError :: TaggedError s
                           }

-- | Only shows 'finalError' to avoid cluttering the entire output.
instance (TokenStream s, Show s, Show (Stream s), Show (Token s)) => Show (ParsingErrors s) where
  showsPrec d = showsPrec d . finalError

instance (TokenStream s, NFData s, NFData (Stream s), NFData (Token s)) => NFData (ParsingErrors s) where
  rnf = rnf . completeLog

-- | The complete log of errors from parsing.
completeLog :: ParsingErrors s -> [TaggedError s]
completeLog = liftA2 getLog errorLog ((:[]) . finalError)

-- | Create a pretty-printed version of the log.
prettyLog :: (ParseInput s) => ParsingErrors s -> String
prettyLog = render . prettyLogElems OnlyErrors . completeLog

prettyDetailedLog :: (ParseInput s) => ParsingErrors s -> String
prettyDetailedLog = render . prettyLogElems ErrorAndCurrentInput . completeLog

-- -----------------------------------------------------------------------------
-- Changing input type.

-- These aren't fmap definitions due to the requirement of having
-- three different functions.

streamToDoc :: (TokenStream s) => [TaggedError s] -> [TaggedError Doc]
streamToDoc = mapStreamTagged prettyValue prettyValue prettyValue

changeStreamError :: (TokenStream s, TokenStream t) => (s -> t) -> (Stream s -> Stream t)
                       -> (Token s -> Token t) -> ParseError s -> ParseError t
changeStreamError finp fstr ftok err
  = case err of
      UnexpectedEndOfInput  -> UnexpectedEndOfInput
      NoMoreInputExpected   -> NoMoreInputExpected
      ExpectedEndOfInput    -> ExpectedEndOfInput
      ExpectedButFound e f  -> ExpectedButFound (ftok e) (ftok f)
      UnexpectedToken t     -> UnexpectedToken (ftok t)
      MissingItemCount n    -> MissingItemCount n
      MissingBracket b      -> MissingBracket b
      ListWithTerminator    -> ListWithTerminator
      Message msg           -> Message msg
      NoParserSatisfied     -> NoParserSatisfied
      PredicateNotSatisfied -> PredicateNotSatisfied
      ParserName nm         -> ParserName nm
      Reparse s             -> Reparse (fstr s)
      Committed             -> Committed
      Backtrack bl          -> Backtrack (mapStreamTagged finp fstr ftok bl)
      NamedSubLogs nsls     -> NamedSubLogs $ map (second $ mapStreamTagged finp fstr ftok) nsls
      ChainedParser         -> ChainedParser
      SubLog sl             -> SubLog sl
      AwaitingInput         -> AwaitingInput
      LogRequested          -> LogRequested

changeStreamTagged :: (TokenStream s, TokenStream t) => (s -> t) -> (Stream s -> Stream t)
                      -> (Token s -> Token t) -> TaggedError s -> TaggedError t
changeStreamTagged finp fstr ftok te
  = TE { parseError    = changeStreamError finp fstr ftok (parseError te)
       , errorLocation = finp (errorLocation te)
       }

mapStreamTagged :: (TokenStream s, TokenStream t) => (s -> t) -> (Stream s -> Stream t)
                   -> (Token s -> Token t) -> [TaggedError s] -> [TaggedError t]
mapStreamTagged finp fstr ftok = map (changeStreamTagged finp fstr ftok)

-- -----------------------------------------------------------------------------

-- | How much information should be printed in error logs.
data LogDetail = OnlyErrors
               | ErrorAndCurrentInput
               -- ^ Also include the current input at the error
               --   location.
               deriving (Eq, Ord, Show, Read)

-- | Internal class to abstract out between 'TaggedError' and
--   'ParseError' for pretty-printing logs.
class PrettyLog e where
  prettyLogElem :: LogDetail -> e -> Doc

instance (TokenStream s) => PrettyLog (TaggedError s) where
  prettyLogElem ld (TE e el) = withInp $ prettyLogElem ld e
    where
      withInp pe
        = case ld of
            OnlyErrors           -> pe
            ErrorAndCurrentInput -> indentLine pe
                                      (text "Input:" <+> prettyValue el)

instance (TokenStream s) => PrettyLog (ParseError s) where
  prettyLogElem _  UnexpectedEndOfInput   = text "Input ended before expected"
  prettyLogElem _  NoMoreInputExpected    = text "No more input available"
  prettyLogElem _  ExpectedEndOfInput     = text "Input not yet empty"
  prettyLogElem _  (ExpectedButFound e f) = text "Expected token" <+> prettyValue e <+> text "but found" <+> prettyValue f
  prettyLogElem _  (UnexpectedToken t)    = text "The next token" <+> prettyValue t <+> text "was not what was expected"
  prettyLogElem _  (MissingItemCount c)   = text "Still expected" <+> int c <+> text "more values"
  prettyLogElem _  (MissingBracket b)     = text "Missing" <+> prettyValue b <+> text "bracket"
  prettyLogElem _  ListWithTerminator     = text "In a list of items with a terminator"
  prettyLogElem _  (Message str)          = text str
  prettyLogElem _  NoParserSatisfied      = text "None of the specified parsers succeeded"
  prettyLogElem _  PredicateNotSatisfied  = text "The supplied predicate was not satisfied"
  prettyLogElem _  (ParserName f)         = text "In the parser combinator" <+> doubleQuotes (text f)
  prettyLogElem _  (Reparse s)            = text "Adding" <+> prettyValue s <+> text "to the front of the parse input"
  prettyLogElem _  Committed              = text "Parser is now committed; unable to backtrack past this point"
  prettyLogElem ld (Backtrack bl)         = text "Backtracking due to parse error:" `indentLine` prettyLogElems ld bl
  prettyLogElem ld (NamedSubLogs nl)      = text "Named alternatives:" `indentLine` bulletList (map (nmSubLog ld) nl)
  prettyLogElem _  ChainedParser          = text "Running a chained parser"
  prettyLogElem ld (SubLog sl)            = text "Log from sub-parser:" `indentLine` prettyLogElems ld sl
  prettyLogElem _  AwaitingInput          = text "Awaiting more input"
  prettyLogElem _  LogRequested           = text "End of requested log"

prettyLogElems :: (PrettyLog e) => LogDetail -> [e] -> Doc
prettyLogElems ld = bulletList . map (prettyLogElem ld)

indentLine :: Doc -> Doc -> Doc
indentLine l1 l2 = l1 $+$ nest 2 l2

bulletList :: [Doc] -> Doc
bulletList = vcat . map (char '*' <+>)

nmSubLog :: (TokenStream s) => LogDetail -> (String, [TaggedError s]) -> Doc
nmSubLog ld (nm,lg) = (text nm  <> colon) `indentLine` prettyLogElems ld lg

-- -----------------------------------------------------------------------------
