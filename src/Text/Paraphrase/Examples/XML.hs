{-# LANGUAGE OverloadedStrings #-}

{- |
   Module      : Text.Paraphrase.Examples.XML
   Description : Basic XML parser
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : 3-Clause BSD-style
   Maintainer  : Ivan.Miljenovic@gmail.com

This is based on the basic XML parser discussed in Malcom Wallace's
<http://www.cs.york.ac.uk/plasma/publications/pdf/partialparse.pdf paper>
/Partial parsing: combining choice with commitment/.

 -}
module Text.Paraphrase.Examples.XML
  ( -- * Importing required modules
    -- $imports

    -- * Defining our types
    -- $types
    Content (..)
  , Attr
  , TagError (..)
  , Parse

    -- * Basic parsers
    -- $basic
  , name
  , space
  , string
  , text
  , attributes

    -- * Parsing tags
    -- $tags
    -- ** Without commit
  , element
  , endTag
  , content
    -- ** With commit
  , elementC
  , endTagC
  , contentC

    -- * Running the parsers
    -- $running
    -- ** With good input
  , goodInput
    -- ** With malformed input
  , badInput
    -- $logs
  ) where

import Text.Paraphrase

import           Control.Applicative
import           Data.Char
import           Data.Maybe                (fromMaybe)
import           Text.PrettyPrint.HughesPJ ((<+>), (<>))
import qualified Text.PrettyPrint.HughesPJ as PP

import Data.ByteString       (ByteString)
import Data.ByteString.Char8 ()

-- -----------------------------------------------------------------------------

{- $imports

Apart from the obvious import of "Text.Paraphrase", we also need to
import:


  * "Control.Applicative" for various combinators.

  * "Data.Char" (since we're doing 'Char'-based parsing).

  * "Text.PrettyPrint.HughesPJ" for defining how to pretty-print our
    custom error 'TagError'.

  * "Data.ByteString" to provide the source input for our parser.
    Note that we only need to import the datatype itself, not any
    functions.  Usage of 'ByteString's is relatively arbitrary:
    see the documentation for 'Parse' for a longer discussion on this.

  * "Data.ByteString.Char8" for use with @OverloadedStrings@.

  * "Data.Maybe" for 'fromMaybe'.

-}

-- -----------------------------------------------------------------------------

{- $types

Before we can start writing our parsers, we first need to choose the
types we're going to use.

-}

-- | The rather simple XML representation that we're going to use.
data Content = Elem String [Attr] [Content]
             | Text String
             deriving (Eq, Show, Read)

-- | A simple alias just for readability purposes.
type Attr = String

-- | The only logic error (as opposed to parsing error) we can have is
--   when the closing tag doesn't match the input tag.  We also define
--   a case for information purposes.
data TagError = WrongClose String String
              | WithinTag String
              deriving (Eq, Ord, Show, Read)

-- | We need some way of displaying the error to end users.
instance PrettyValue TagError where
  prettyValue (WrongClose open close)
    = PP.text "Opening tag <" <> PP.text open
      <> PP.text "> terminated by </" <> PP.text close <> PP.text ">"
  prettyValue (WithinTag nm)
    = PP.text "Parsing tag:" <+> PP.text nm

-- | Due to the polymorphic nature of the 'Parser' type, it helps to
--   define and use an overall type alias to make explicit what types
--   we're dealing with.
--
--   @'Parser' e s a@ takes three type parameters:
--
--   [@e@] The type of logic\/additional errors we can expect.  If
--         there aren't any, using @()@ is acceptable.
--
--   [@s@] The 'ParseInput' instance that we're using to provide
--         data for our parser.
--
--   [@a@] The actual value this parser produces.
--
--
--   Note that in particular, our choice of source is dependent upon
--   what features we want.  In this case, we want to parse 'Char's,
--   and we choose @'AsChar8' ByteString@ to read in a 'ByteString' as
--   if it was composed of @Char8@ values.  We could change this to
--   @'String'@ or @'Data.Text.Text'@ without changing the actual
--   parser (since we're not actually using any fused features to get
--   actual subsets of the original input).  In real-life usage we
--   would choose more appropriately.
type Parse a = Parser TagError (AsChar8 ByteString) a

-- -----------------------------------------------------------------------------

{- $basic

Simple parsers used as building blocks.

-}

-- | Parsing individual names of items.  We /are/ assuming that we're
--   only dealing with ASCII (or at most Latin1) characters.
name :: Parse String
name = (some (satisfy (liftA2 (&&) (not . isSpace) (`notElem` ['<','>','/'])))) <?> "name"

-- | Parse at least one whitespace character.
space :: Parse ()
space = some (satisfy isSpace) *> pure ()

-- | Parse the specified string.
string :: String -> Parse ()
string str = mapM_ token str <?> "string"

-- | Parse 'Text' values: basically anything that isn't a tag.
text :: Parse Content
text = Text <$> some (satisfy (/= '<')) <?> "text"

-- | Parse a list of attributes for within a tag.
attributes :: Parse [Attr]
attributes = (name `sepBy1` space) <?> "attributes"

-- -----------------------------------------------------------------------------

{- $tags

We define two versions that are identical except for usage of 'commit'
to demonstrate how useful it can be.

-}

-- | A version without commit.  Mutually recursive with 'content'.
--   Calls 'endTag'.
element :: Parse Content
element = (token '<'
   *> do n <- name
         addCustomOnFailure (WithinTag n) (
           do as <- fromMaybe [] <$> optional (space *> attributes)
              ( (string "/>" *> pure (Elem n as []))
                <|>
                (token '>' *> (Elem n as <$> manyFinally' content (endTag n)))
               )
          )) <?> "element"

-- | Called by 'element'; ends a tag with the specified name.
endTag :: String -> Parse ()
endTag n = do addTraceMessage ("Closing tag for: " ++ n)
              m <- bracket (string "</") (token '>') name
              if n == m
                 then pure ()
                 else failCustom (WrongClose n m)

-- | The overall parser.  Mutually recursive with 'element'.
content :: Parse Content
content = element <|> text <|> failWith NoParserSatisfied

addTraceMessage :: String -> Parse ()
addTraceMessage m = addStackTrace (Message m) (pure ())

-- | A version with commit.  Mutually recursive with 'contentC'.  Calls
--   'endTagC'.
elementC :: Parse Content
elementC = token '<'
  *> commit ( do n <- name
                 addCustomOnFailure (WithinTag n) (
                   do as <- fromMaybe [] <$> optional (space *> attributes)
                      ( (string "/>" *> commit (pure (Elem n as [])))
                        <|>
                        (token '>' *> commit (Elem n as <$> manyFinally' contentC (endTagC n)))
                       )
                  )
             )

-- | Called by 'elementC'; ends a tag with the specified name.
endTagC :: String -> Parse ()
endTagC n = do m <- bracket (string "</") (commit $ token '>') name
               if n == m
                  then pure ()
                  else failCustom (WrongClose n m)

-- | The overall (committed) parser.  Mutually recursive with
--   'elementC'.
contentC :: Parse Content
contentC = elementC <|> text <|> failWith NoParserSatisfied

-- -----------------------------------------------------------------------------

{- $running

We run our parsers with 'runParser' (as we're not dealing with
incremental input).  As an alternative, the 'runParser'' function is
useful when testing in @ghci@.

-}

-- | Valid input that should be parsed correctly.
--
--   If we run our two parsers over this, we find that they both parse
--   the same thing:
--
--   >>> runParser content goodInput
--   (Right (Elem "a" [] [Elem "b" [] [Text "hello"],Elem "c" [] [Text "world"]]),AsChar8 {unChar8 = ""})
--
--   >>> runParser contentC goodInput
--   (Right (Elem "a" [] [Elem "b" [] [Text "hello"],Elem "c" [] [Text "world"]]),AsChar8 {unChar8 = ""})
goodInput :: ByteString
goodInput = "<a><b>hello</b><c>world</c></a>"

-- | Malformed XML that shouldn't be parsed correctly.
--
--   What's interesting is /how/ the parser informs the user that the
--   input is malformed.  Running the two parsers over this:
--
--   >>> runParser content badInput
--   (Left (TE {parseError = NoParserSatisfied, errorLocation = AsChar8 {unChar8 = "<a><b>hello<b/></a>"}}),AsChar8 {unChar8 = "<a><b>hello<b/></a>"})
--
--   >>> runParser contentC badInput
--   (Left (TE {parseError = CustomError (WrongClose "b" "a"), errorLocation = AsChar8 {unChar8 = ""}}),AsChar8 {unChar8 = ""})
--
--   The version without 'commit' gives back a completely un-helpful
--   error message, whereas the final error for the committed variant
--   indicates what the problem is.
--
--   Also note the remaining input: the first parser has backtracked
--   all the way to the beginning trying to find /some/ way to parse
--   the input, whereas the second has consumed the entire input,
--   determined the input is malformed and then given up.
badInput :: ByteString
badInput = "<a><b>hello<b/></a>"

{- $logs

As an extended example, here's what the pretty-printed entire log for
the committed parser looks like:

>>> runParser' contentC badInput
*** Exception:
* Parser is now committed; unable to backtrack past this point
* Parsing tag: a
* Backtracking due to parse error:
    * In the parser combinator "string"
    * Expected token '/' but found '>'
* Parser is now committed; unable to backtrack past this point
* In a list of items with a terminator
* Backtracking due to parse error:
    * Missing opening bracket
    * In the parser combinator "string"
    * Expected token '/' but found 'b'
* Parser is now committed; unable to backtrack past this point
* Parsing tag: b
* Backtracking due to parse error:
    * In the parser combinator "string"
    * Expected token '/' but found '>'
* Parser is now committed; unable to backtrack past this point
* In a list of items with a terminator
* Backtracking due to parse error:
    * Missing opening bracket
    * In the parser combinator "string"
    * Expected token '<' but found 'h'
* Backtracking due to parse error:
    * Missing opening bracket
    * In the parser combinator "string"
    * Expected token '/' but found 'b'
* Parser is now committed; unable to backtrack past this point
* Opening tag <b> terminated by </a>

-}
