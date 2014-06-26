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
  , MapError (..)
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

import Text.Paraphrase.Inputs (ParseInput (..), PrettyInput (..),
                               TokenStream (..))
import Text.Paraphrase.Pretty

import Text.PrettyPrint.HughesPJ hiding (isEmpty, (<>))

import           Control.Applicative (liftA2)
import           Control.Arrow       (second)
import           Control.DeepSeq     (NFData (rnf))
import qualified Data.DList          as DL
import           Data.Function       (on)
import           Data.Monoid
import           Data.String         (IsString (..))

-- -----------------------------------------------------------------------------

class MapError c where
  convertErrorBy :: (e -> e') -> c e s -> c e' s

-- -----------------------------------------------------------------------------

-- | The possible errors that could arise when parsing.  Please note
--   that to an extent, \"errors\" is a bit of a misnomer, as these
--   are also used for general logging whilst parsing, though these
--   logs are only provided when an actual error arises.
data ParseError e s
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
  | Backtrack (ParsingErrors e s)        -- ^ The log from the left-hand-side of '\<|\>'.
  | NamedSubLogs [(String, ParsingErrors e s)]
  | ChainedParser
  | SubLog (ParsingErrors e PrettyInput) -- ^ The log (if any) from a chained parser.
  | AwaitingInput                        -- ^ Within a 'Partial' result type.
  | LogRequested                         -- ^ Used with "Text.Paraphrase.Debug".
  | CustomError e                        -- ^ Able to provide a user-specific error.

-- Need to make these instances separate for GHC to be happy; hence
-- also the various language pragmas above.
deriving instance (TokenStream s, Eq   s, Eq   (Stream s), Eq   (Token s), Eq   e) => Eq   (ParseError e s)
deriving instance (TokenStream s, Show s, Show (Stream s), Show (Token s), Show e) => Show (ParseError e s)

instance (TokenStream s, NFData s, NFData (Stream s), NFData (Token s), NFData e) => NFData (ParseError e s) where
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

instance IsString (ParseError e s) where
 fromString = Message

instance MapError ParseError where
  convertErrorBy _ UnexpectedEndOfInput   = UnexpectedEndOfInput
  convertErrorBy _ NoMoreInputExpected    = NoMoreInputExpected
  convertErrorBy _ ExpectedEndOfInput     = ExpectedEndOfInput
  convertErrorBy _ (ExpectedButFound e f) = ExpectedButFound e f
  convertErrorBy _ (UnexpectedToken t)    = UnexpectedToken t
  convertErrorBy _ (MissingItemCount n)   = MissingItemCount n
  convertErrorBy _ (MissingBracket b)     = MissingBracket b
  convertErrorBy _ ListWithTerminator     = ListWithTerminator
  convertErrorBy _ (Message msg)          = Message msg
  convertErrorBy _ NoParserSatisfied      = NoParserSatisfied
  convertErrorBy _ PredicateNotSatisfied  = PredicateNotSatisfied
  convertErrorBy _ (ParserName nm)        = ParserName nm
  convertErrorBy _ (Reparse s)            = Reparse s
  convertErrorBy _ Committed              = Committed
  convertErrorBy f (Backtrack bl)         = Backtrack (convertErrorBy f bl)
  convertErrorBy f (NamedSubLogs nsls)    = NamedSubLogs (map (second (convertErrorBy f)) nsls)
  convertErrorBy _ ChainedParser          = ChainedParser
  convertErrorBy f (SubLog sl)            = SubLog (convertErrorBy f sl)
  convertErrorBy _ AwaitingInput          = AwaitingInput
  convertErrorBy _ LogRequested           = LogRequested
  convertErrorBy f (CustomError e)        = CustomError (f e)

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
data TaggedError e s = TE { parseError    :: !(ParseError e s)
                            -- ^ The error that arose.
                          , errorLocation :: !s
                            -- ^ The current input when the error arose.
                          }

deriving instance (Eq   (ParseError e s), Eq   s) => Eq   (TaggedError e s)
deriving instance (Show (ParseError e s), Show s) => Show (TaggedError e s)

instance (TokenStream s, NFData s, NFData (Stream s), NFData (Token s), NFData e) => NFData (TaggedError e s) where
  rnf (TE pe el) = rnf pe `seq` rnf el

instance MapError TaggedError where
  convertErrorBy f te = te { parseError = convertErrorBy f (parseError te) }

newtype ParseLog e s = PL { getLog :: DL.DList (TaggedError e s) }

deriving instance (Eq   (TaggedError e s)) => Eq   (ParseLog e s)
deriving instance (Show (TaggedError e s)) => Show (ParseLog e s)

instance (NFData (TaggedError e s)) => NFData (ParseLog e s) where
  rnf = rnf . getLog

instance Monoid (ParseLog e s) where
  mempty = PL mempty

  mappend (PL l1) (PL l2) = PL (l1 <> l2)

instance MapError ParseLog where
  convertErrorBy f = PL . fmap (convertErrorBy f) . getLog

-- | Add a @ParseError@ to the end of the log.
logError :: ParseLog e s -> ParseError e s -> s -> ParseLog e s
logError pl e inp = pl { getLog = getLog pl `DL.snoc` (TE e inp) }

createFinalLog :: ParseLog e s -> ParseError e s -> s -> ParsingErrors e s
createFinalLog pl e inp = PEs pl (TE e inp)

-- | The log of errors from parsing.
data ParsingErrors e s = PEs { errorLog   :: ParseLog e s
                             -- | The error which caused the parsing to
                             -- fail.
                             , finalError :: TaggedError e s
                             }

instance (Eq (TaggedError e s)) => Eq (ParsingErrors e s) where
  (==) = (==) `on` completeLog

-- | Only shows 'finalError' to avoid cluttering the entire output.
instance (Show (TaggedError e s)) => Show (ParsingErrors e s) where
  showsPrec d = showsPrec d . finalError

instance (TokenStream s, NFData s, NFData (Stream s), NFData (Token s), NFData e) => NFData (ParsingErrors e s) where
  rnf = rnf . completeLog

instance MapError ParsingErrors where
  convertErrorBy f pe = PEs { errorLog   = convertErrorBy f (errorLog pe)
                            , finalError = convertErrorBy f (finalError pe)
                            }

-- | The complete log of errors from parsing.
completeLog :: ParsingErrors e s -> [TaggedError e s]
completeLog = DL.toList . liftA2 DL.snoc (getLog . errorLog) finalError

-- | Create a pretty-printed version of the log.
prettyLog :: (ParseInput s, PrettyValue e) => ParsingErrors e s -> String
prettyLog = render . prettyLogElems OnlyErrors . completeLog

prettyDetailedLog :: (ParseInput s, PrettyValue e) => ParsingErrors e s -> String
prettyDetailedLog = render . prettyLogElems ErrorAndCurrentInput . completeLog

-- -----------------------------------------------------------------------------
-- Changing input type.

-- These aren't fmap definitions due to the requirement of having
-- three different functions.

streamToDoc :: (TokenStream s) => ParsingErrors e s -> ParsingErrors e PrettyInput
streamToDoc = changeStreamErrors prettyInput prettyValue prettyValue

changeStreamError :: (TokenStream s, TokenStream t) => (s -> t) -> (Stream s -> Stream t)
                       -> (Token s -> Token t) -> ParseError e s -> ParseError e t
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
      Backtrack bl          -> Backtrack (changeStreamErrors finp fstr ftok bl)
      NamedSubLogs nsls     -> NamedSubLogs $ map (second $ changeStreamErrors finp fstr ftok) nsls
      ChainedParser         -> ChainedParser
      SubLog sl             -> SubLog sl
      AwaitingInput         -> AwaitingInput
      LogRequested          -> LogRequested
      CustomError e         -> CustomError e

changeStreamTagged :: (TokenStream s, TokenStream t) => (s -> t) -> (Stream s -> Stream t)
                      -> (Token s -> Token t) -> TaggedError e s -> TaggedError e t
changeStreamTagged finp fstr ftok te
  = TE { parseError    = changeStreamError finp fstr ftok (parseError te)
       , errorLocation = finp (errorLocation te)
       }

changeStreamLog :: (TokenStream s, TokenStream t) => (s -> t) -> (Stream s -> Stream t)
                   -> (Token s -> Token t) -> ParseLog e s -> ParseLog e t
changeStreamLog finp fstr ftok = PL . fmap (changeStreamTagged finp fstr ftok) . getLog

changeStreamErrors :: (TokenStream s, TokenStream t) => (s -> t) -> (Stream s -> Stream t)
                      -> (Token s -> Token t) -> ParsingErrors e s -> ParsingErrors e t
changeStreamErrors finp fstr ftok pe = PEs { errorLog   = changeStreamLog finp fstr ftok
                                                                          (errorLog pe)
                                           , finalError = changeStreamTagged finp fstr ftok
                                                                             (finalError pe)
                                           }

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

instance (TokenStream s, PrettyValue e) => PrettyLog (TaggedError e s) where
  prettyLogElem ld (TE e el) = withInp $ prettyLogElem ld e
    where
      withInp pe
        = case ld of
            OnlyErrors           -> pe
            ErrorAndCurrentInput -> indentLine pe
                                      (text "Input:" <+> pStream (prettyInput el))

instance (TokenStream s, PrettyValue e) => PrettyLog (ParseError e s) where
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
  prettyLogElem ld (Backtrack bl)         = text "Backtracking due to parse error:" `indentLine` prettyLogElem ld bl
  prettyLogElem ld (NamedSubLogs nl)      = text "Named alternatives:" `indentLine` bulletList (map (nmSubLog ld) nl)
  prettyLogElem _  ChainedParser          = text "Running a chained parser"
  prettyLogElem ld (SubLog sl)            = text "Log from sub-parser:" `indentLine` prettyLogElem ld sl
  prettyLogElem _  AwaitingInput          = text "Awaiting more input"
  prettyLogElem _  LogRequested           = text "End of requested log"
  prettyLogElem _  (CustomError e)        = prettyValue e

instance (TokenStream s, PrettyValue e) => PrettyLog (ParsingErrors e s) where
  prettyLogElem ld = prettyLogElems ld . completeLog

prettyLogElems :: (PrettyLog e) => LogDetail -> [e] -> Doc
prettyLogElems ld = bulletList . map (prettyLogElem ld)

indentLine :: Doc -> Doc -> Doc
indentLine l1 l2 = l1 $+$ nest 2 l2

bulletList :: [Doc] -> Doc
bulletList = vcat . map (char '*' <+>)

nmSubLog :: (TokenStream s, PrettyValue e) => LogDetail
            -> (String, ParsingErrors e s) -> Doc
nmSubLog ld (nm,lg) = (text nm  <> colon) `indentLine` prettyLogElem ld lg

-- -----------------------------------------------------------------------------
