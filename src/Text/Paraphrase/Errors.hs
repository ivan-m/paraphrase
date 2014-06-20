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
  , TaggedError
  , parseError
  , errorLocation
  , ParseLog -- Constructor not exported!
  , logError
  , ParsingErrors
  , createFinalLog
  , finalError
  , completeLog
  , prettyLog
  ) where

import Text.Paraphrase.Inputs (ParseInput (..), TokenStream (..))

import Control.Applicative (liftA2)
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
  | ExpectedEndOfInput
  | ExpectedButFound (Token s) (Token s) -- ^ The token that was expected/required.
  | UnexpectedToken (Token s)            -- ^ Token found that did not match what was required/expected.
  | MissingItemCount Int                 -- ^ Expected this many more items that were not found.
  | Message String                       -- ^ Can be created via the @OverloadedStrings@ pragma.
  | ParserName String                    -- ^ Used with '<?>'.
  | Reparse (Stream s)                   -- ^ Additional input added to front.
  | Committed
  | NamedSubLogs [(String, [TaggedError s])]
  | SubLog [TaggedError s]
  | AwaitingInput                        -- ^ Within a 'Partial' result type.
  | LogRequested                         -- ^ Used with "Text.Paraphrase.Debug".

-- Need to make these instances separate for GHC to be happy; hence
-- also the various language pragmas above.
deriving instance (TokenStream s, Eq   s, Eq   (Stream s), Eq   (Token s)) => Eq   (ParseError s)
deriving instance (TokenStream s, Ord  s, Ord  (Stream s), Ord  (Token s)) => Ord  (ParseError s)
deriving instance (TokenStream s, Show s, Show (Stream s), Show (Token s)) => Show (ParseError s)
deriving instance (TokenStream s, Read s, Read (Stream s), Read (Token s)) => Read (ParseError s)

-- | Orphan instance needed for 'ParseError's instance.
instance NFData Doc where
  rnf = rnf . render

instance Eq Doc where
  (==) = (==) `on` render

instance (TokenStream s, NFData s, NFData (Stream s), NFData (Token s)) => NFData (ParseError s) where
  rnf (ExpectedButFound e f) = rnf e `seq` rnf f
  rnf (UnexpectedToken t)    = rnf t
  rnf (MissingItemCount n)   = rnf n
  rnf (Message msg)          = rnf msg
  rnf (ParserName nm)        = rnf nm
  rnf (Reparse s)            = rnf s
  rnf (NamedSubLogs spls)    = rnf spls
  rnf (SubLog pl)            = rnf pl
  rnf _                      = ()

instance IsString (ParseError s) where
 fromString = Message

-- | A 'ParseError' tagged with the current input at the location of
--   where it arose.
data TaggedError s = TE { parseError    :: !(ParseError s)
                          -- ^ The error that arose.
                        , errorLocation :: !s
                          -- ^ The current input when the error arose.
                        }

deriving instance (TokenStream s, Eq   s, Eq   (Stream s), Eq   (Token s)) => Eq   (TaggedError s)
deriving instance (TokenStream s, Ord  s, Ord  (Stream s), Ord  (Token s)) => Ord  (TaggedError s)
deriving instance (TokenStream s, Show s, Show (Stream s), Show (Token s)) => Show (TaggedError s)
deriving instance (TokenStream s, Read s, Read (Stream s), Read (Token s)) => Read (TaggedError s)

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
prettyLog :: (ParseInput s, Show s, Show (Token s)) => ParsingErrors s -> String
prettyLog = unlines . map (show . parseError) . completeLog
