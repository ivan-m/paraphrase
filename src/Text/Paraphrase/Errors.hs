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
  , ParseLog -- Constructor not exported!
  , (|>)
  , ParsingErrors
  , createFinalLog
  , finalError
  , completeLog
  , prettyLog
  ) where

import Text.Paraphrase.Inputs (ParseInput (..))

import Control.Applicative (liftA2)
import Control.DeepSeq     (NFData (rnf))
import Data.Function       (on)
import Data.Monoid
import Data.String         (IsString (..))

-- -----------------------------------------------------------------------------

-- | The possible errors that could arise when parsing.
data ParseError s
  = UnexpectedEndOfInput
  | NoMoreInputExpected -- ^ When more input requested after being told there isn't any more.
  | ExpectedEndOfInput s
  | ExpectedButFound (Token s) (Token s) -- ^ The token that was expected/required.
  | UnexpectedToken (Token s)            -- ^ Token found that did not match what was required/expected.
  | MissingItemCount Int s               -- ^ Expected this many more items that were not found.
  | Message String                       -- ^ Can be created via the @OverloadedStrings@ pragma.
  | Committed
  | NamedSubLogs [(String, [ParseError s])]
  | SubLog [ParseError s]
  | AwaitingInput                        -- ^ Within a 'Partial' result type.

-- Need to make these instances separate for GHC to be happy; hence
-- also the various language pragmas above.
deriving instance (ParseInput s, Eq   s, Eq   (Token s)) => Eq   (ParseError s)
deriving instance (ParseInput s, Ord  s, Ord  (Token s)) => Ord  (ParseError s)
deriving instance (ParseInput s, Show s, Show (Token s)) => Show (ParseError s)
deriving instance (ParseInput s, Read s, Read (Token s)) => Read (ParseError s)

instance (ParseInput s, NFData s, NFData (Token s)) => NFData (ParseError s) where
  rnf (ExpectedEndOfInput s) = rnf s
  rnf (ExpectedButFound e f) = rnf e `seq` rnf f
  rnf (UnexpectedToken t)    = rnf t
  rnf (MissingItemCount n s) = rnf n `seq` rnf s
  rnf (Message msg)          = rnf msg
  rnf (NamedSubLogs spls)    = rnf spls
  rnf (SubLog pl)            = rnf pl
  rnf _                      = ()

instance IsString (ParseError s) where
 fromString = Message


newtype ParseLog s = PL { getLog :: [ParseError s] -> [ParseError s]  }

instance (ParseInput s, Eq s, Eq (Token s)) => Eq (ParseLog s) where
  (==) = (==) `on` ($[]) . getLog

instance (ParseInput s, Show s, Show (Token s)) => Show (ParseLog s) where
  showsPrec d = showsPrec d . ($[]) . getLog

instance Monoid (ParseLog s) where
  mempty = PL id

  mappend (PL l1) (PL l2) = PL (l1 . l2)

-- | Add a @ParseError@ to the end of the log.
(|>) :: ParseLog s -> ParseError s -> ParseLog s
(|>) pl e = pl { getLog = getLog pl . (e:) }

infixl 5 |>

createFinalLog :: ParseLog s -> ParseError s -> ParsingErrors s
createFinalLog = PEs

-- | The log of errors from parsing.
data ParsingErrors s = PEs { errorLog   :: ParseLog s
                           -- | The error which caused the parsing to
                           -- fail.
                           , finalError :: ParseError s
                           }

-- | Only shows 'finalError' to avoid cluttering the entire output.
instance (ParseInput s, Show s, Show (Token s)) => Show (ParsingErrors s) where
  showsPrec d = showsPrec d . finalError

instance (ParseInput s, NFData s, NFData (Token s)) => NFData (ParsingErrors s) where
  rnf = rnf . completeLog

-- | The complete log of errors from parsing.
completeLog :: ParsingErrors s -> [ParseError s]
completeLog = liftA2 getLog errorLog ((:[]) . finalError)

-- | Create a pretty-printed version of the log.
prettyLog :: (ParseInput s, Show s, Show (Token s)) => ParsingErrors s -> String
prettyLog = unlines . map show . completeLog
