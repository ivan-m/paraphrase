{-# LANGUAGE ConstraintKinds, DefaultSignatures, FlexibleContexts, TypeFamilies
             #-}
{- |
   Module      : Text.Paraphrase.Inputs
   Description : Defining possible inputs for parsing
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

   This module should only be imported if\/when you want to create a
   new instance of 'ParseInput' (either because you have a new type of
   input you want to parse, or to create a new wrapper type).

   There is no need to view this module if you only want to use
   existing instances to write parsers.

 -}
module Text.Paraphrase.Inputs
  ( -- * Streams and Tokens
    TokenStream (..)
  , BaseStream
    -- ** Pretty-printing support
  , PrettyValue (..)
  , PrettyInput
  , pInputs
  , pStream
  , prettyStream
  , addPrettyInput

    -- * Parse-able values
  , ParseInput (..)
  ) where

import Text.Paraphrase.Pretty

import qualified Data.ByteString           as SB
import qualified Data.ByteString.Lazy      as LB
import qualified Data.ByteString.Unsafe    as SB
import qualified Data.Text                 as ST
import qualified Data.Text.Lazy            as LT
import qualified Data.Text.Unsafe          as ST
import           Data.Word                 (Word8)
import           Text.PrettyPrint.HughesPJ (Doc, render)

import           Control.Applicative (liftA2)
import           Control.DeepSeq     (NFData (rnf))
import           Data.Function       (on)
import qualified Data.ListLike       as LL

-- -----------------------------------------------------------------------------

-- | This class distinguishes between three concepts:
--
--   * The input type provided to a parser @s@.
--
--   * The underlying values being parsed @Stream s@ (in many cases
--     @Stream s ~ s@; if instead @s@ is a wrapper type then you will
--     need to define this yourself).
--
--   * The individual elements which compose the input, @Token s@.
--
--   This class is separated from 'ParseInput' so as to be able to
--   provide a minimum requirement for some definitions.
class (PrettyValue (Stream s), PrettyValue (Token s))
      => TokenStream s where

  -- | The actual input stream.  In many cases @Stream s ~ s@
  --   (especially for base values to be parsed); if this is the case
  --   then no definition is required..
  type Stream s
  type Stream s = s

  -- | The elements of this input type.
  type Token s

  -- | How to pretty-print a value such that it's possible to choose
  --   whether to print just the 'Stream' or the entire input.
  --
  --   When @Stream s ~ s@ this is defined for you (as being just
  --   'prettyStream'); otherwise, recurse down the stack (optionally
  --   adding a value with 'addPrettyInput').
  prettyInput :: s -> PrettyInput
  default prettyInput :: (Stream s ~ s) => s -> PrettyInput
  prettyInput = prettyStream

-- | The structured pretty representation of a @TokenStream@ instance.
data PrettyInput = PI { pInputs :: [(String, Doc)]
                        -- ^ Any additional pretty-printing
                        --   information from potential wrappers
                        --   around the base 'Stream'.
                      , pStream :: !Doc
                        -- ^ The pretty-printing representation of the
                        --   raw 'Stream' from the original input.
                        --   Equivalent to running @'prettyValue'
                        --   . 'getStream'@ on an instance of
                        --   'ParseInput'.
                      }
                   deriving (Eq, Show)

-- | Construct a @PrettyInput@ value when we reach the raw 'Stream'.
prettyStream :: (TokenStream s, s ~ Stream s) => s -> PrettyInput
prettyStream s = PI { pInputs = []
                    , pStream = prettyValue s
                    }

-- | Optionally use this when we have a wrapper over a raw 'Stream'.
--   The 'String' argument is used to provide an identifier when
--   printing error messages.
addPrettyInput :: (String, Doc) -> PrettyInput -> PrettyInput
addPrettyInput sd pin = pin { pInputs = sd : pInputs pin }

instance TokenStream PrettyInput where
  type Stream PrettyInput = Doc

  type Token  PrettyInput = Doc

  prettyInput = id

instance NFData PrettyInput where
  rnf = liftA2 seq (rnf . pInputs) (rnf . pStream)

-- -----------------------------------------------------------------------------

-- | What we expect all base-level 'ParseInput' values to look like.
--
--   The 'LL.ListLike' requirement is used to provide many common
--   definitions (rather than re-defining them here).
type BaseStream s = (TokenStream s, Stream s ~ s, LL.ListLike s (Token s))

-- | The types which we know how to manipulate at a low-level.  This
--   class defines the minimum that is required to use all parser
--   combinators.
--
--   Unless you're defining a new input source, you probably do not
--   need to examine the methods of this class.
--
--   If you are defining a new wrapper type (i.e. it is /not/ true
--   that @'Stream' s ~ s@), then make sure you provide definitions
--   for 'getStream', 'fromStream', 'prependStream' and 'appendStream'
--   (otherwise you'll get type definition warnings).
class (TokenStream s, BaseStream (Stream s)) => ParseInput s where

  -- | The current stream stored in this input value.  Pre-defined
  --   when @Stream s ~ s@.
  getStream :: s -> Stream s
  default getStream :: (Stream s ~ s) => s -> s
  getStream = id
  {-# INLINE getStream #-}

  -- | Create a new input value from the provided stream.  As such, a
  --   blank input value will be equivalent to @fromStream 'LL.empty'@.
  --   Pre-defined when @Stream s ~ s@.
  fromStream :: Stream s -> s
  default fromStream :: (Stream s ~ s) => s -> s
  fromStream = id
  {-# INLINE fromStream #-}

  -- | Get (as in consume) the current stream and set the stream in
  --   the input to @'LL.empty'@.  Default provided for values that
  --   are their own Stream.
  --
  --   This is used for parser chaining.
  --
  --   This differs from @\ s -> (getStream s, fromStream 'LL.empty')
  --   in cases where something stateful is stored in the input.
  extractCurrentStream :: s -> (Stream s,s)
  default extractCurrentStream :: (Stream s ~ s) => s -> (s,s)
  extractCurrentStream = flip (,) LL.empty
  {-# INLINE extractCurrentStream #-}

  -- | Add new @Stream@ value to the front.  Default provided for
  --   values that are their own Stream.
  --
  --   This is used by the 'reparse' combinator and for parser
  --   chaining.
  prependStream :: Stream s -> s -> s
  default prependStream :: (Stream s ~ s) => s -> s -> s
  prependStream = LL.append
  {-# INLINE prependStream #-}

  -- | Add new @Stream@ value to the back.  Default provided for
  --   values that are their own Stream.
  --
  --   This is used when additional input is requested.
  appendStream :: s -> Stream s ->  s
  default appendStream :: (Stream s ~ s) => s -> s -> s
  appendStream = LL.append
  {-# INLINE appendStream #-}

  -- | Obtain the first token in the input.  Only used when the input
  --   isn't empty, honest!  Default provided for values that are
  --   their own Stream.
  inputHead :: s -> Token s
  default inputHead :: (Stream s ~ s) => s -> Token s
  inputHead = LL.head
  {-# INLINE inputHead #-}

  -- | Return all but the first token of the input.  Only used when
  --   the input isn't empty, honest!  Default provided for values
  --   that are their own Stream.
  inputTail :: s -> s
  default inputTail :: (Stream s ~ s) => s -> s
  inputTail = LL.tail
  {-# INLINE inputTail #-}

  -- | Do we have at least @n@ tokens available?  Default provided for
  --   values that are their own Stream (which assumes @O(1)@
  --   'LL.length' calculation).
  lengthAtLeast :: s -> Int -> Bool
  default lengthAtLeast :: (Stream s ~ s) => s -> Int -> Bool
  lengthAtLeast s n = LL.length s >= n
  {-# INLINE lengthAtLeast #-}

  -- | Split the stream where the predicate is no longer satisfied
  --   (that is, the @fst@ component contains the largest possible
  --   prefix where all values satisfy the predicate, and the @snd@
  --   component contains the remainder of the input).  Default
  --   provided for values that are their own Stream.
  breakWhen :: (Token s -> Bool) -> s -> (Stream s,s)
  default breakWhen :: (Stream s ~ s) => (Token s -> Bool) -> s -> (s,s)
  breakWhen = LL.span
  {-# INLINE breakWhen #-}

  -- | Return the stream containing the first @n@ tokens.  It is safe
  --   to assume that the length of the current input is @>= n@.
  --   Default provided for values that are their own Stream.
  getStreamLength :: Int -> s -> (Stream s,s)
  default getStreamLength :: (Stream s ~ s) => Int -> s -> (s,s)
  getStreamLength = LL.splitAt
  {-# INLINE getStreamLength #-}

-- -----------------------------------------------------------------------------
-- Instances for various concrete types.

instance (PrettyValue a) => TokenStream [a] where
  type Token [a] = a

instance (PrettyValue a) => ParseInput [a] where

  lengthAtLeast as n = not . null . drop (n-1) $ as
  {-# INLINE lengthAtLeast #-}

instance TokenStream SB.ByteString where
  type Token SB.ByteString = Word8

instance ParseInput SB.ByteString where

  -- Keep using the unsafe variants for a tad more performance.

  inputHead = SB.unsafeHead
  {-# INLINE inputHead #-}

  inputTail = SB.unsafeTail
  {-# INLINE inputTail #-}

instance TokenStream LB.ByteString where
  type Token LB.ByteString = Word8

instance ParseInput LB.ByteString
  -- length is O(n), but there's no better way of doing it so use the
  -- default. for lengthAtLeast

instance TokenStream ST.Text where
  type Token ST.Text = Char

instance ParseInput ST.Text where

  -- Keep using the unsafe variants for a tad more performance.

  inputHead = ST.unsafeHead
  {-# INLINE inputHead #-}

  inputTail = ST.unsafeTail
  {-# INLINE inputTail #-}

  -- We do @`quot` 2@ because UTF-16 (which Text is implemented with)
  -- code points are either 1 or 2 Word16 values.  As such do this
  -- O(1) test first in case it suffices before we do the O(n) case
  -- for the real length.
  lengthAtLeast t n = (ST.lengthWord16 t `quot` 2) >= n || ST.length t >= n
  {-# INLINE lengthAtLeast #-}

instance TokenStream LT.Text where
  type Token LT.Text = Char

instance ParseInput LT.Text
  -- length is O(n), but there's no better way of doing it so use the
  -- default. for lengthAtLeast
