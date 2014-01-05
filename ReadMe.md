% polyparse-next
% Ivan Lazar Miljenovic

You don't want to use this library.

Really.

This is an experiment in terms of how much performance can be squeezed
out of a parser using an API like that of _[polyparse]_ (specifically
the `Text.ParserCombinators.Poly.*` modules) by using an
implementation based upon that of _[attoparsec]_'s.

If this is successful, it will either be folded into polyparse, or
else into a new improved parser combinator library (with a better name
than "polyparse-next").

[polyparse]: http://hackage.haskell.org/package/polyparse

[attoparsec]: http://hackage.haskell.org/package/attoparsec

Goals of this library/experiment
================================

* Simple API (both in terms of available combinators and available
  modules).

* Fast.

* Well-documented, both in terms of Haddock documentation and also
  internal documentation to help understand how everything works.

* Separation of concerns: try and separate the concept of incremental
  parsing from the values being parsing, and make the library as
  polymorphic as possible.

* Maximise re-use: no need to re-implement combinators for every
  single type that might want to be parsed.

* Available for use for high-level parsing as well (e.g. a JSON
  parser).

Why not just use attoparsec?
============================

_attoparsec_ is the current favourite of the various Haskell parser
combinator libraries, but whilst it definitely wins in terms of speed,
there are some aspects of it I don't particularly like.  For more
details, see below.

Analysis of existing parsing combinator libraries
=================================================

polyparse
---------

### Positives

* Relatively simple API without needing to look through various
  modules to find it.

* Available for a multitude of input types.

* Ability to add adjust error messages to be able to provide more
  informative message.

* Ability to have a parser with state support.

### Negatives

* Lack of specialised combinators that support fusion, etc. to improve
  performance.

* Some combinators are duplicates of existing type-class
  methods/operators.

* Can be difficult to find the correct module to use due to other
  parsers in the library, etc.

* Each actual parser is unique to each input type rather than taking
  the input type as a parametric value.

* State support is baked into extra defined parsers rather than using
  existing `StateT` transformers, etc.

    - These extra parsers also mean that combinators are also defined
      for each separate parser by convention rather than enforced by
      the type classes.

attoparsec
----------

### Positives

* Fast.  This is due to a combination of being implemented as a chain
  of functions as well as emphasis on fusion-capable combinators.

* Support for incremental parsing.

* Good Haddock documentation.

### Negatives

* Many combinators are re-implemented for every possible input type.

    - Others are made generic, but rely on GHC's `SPECIALIZE` pragma
      for performance.

* Still have to choose the module based upon what you're parsing.

* Poor internal documentation (e.g. of what the various fields in the
  parser are or how the various low-level combinators work).

* No support for monad transformers or any kind of stateful parsing.

* Despite using backtracking-by-default, no support for anything like
  polyparse's `commit` combinators.

    - This shouldn't be too difficult to add in though (as it was
      implemented for this library).

### Other points

* Rather than being able to manipulate the error message, parsers are
  able to be tagged with a name and a result of `Fail` contains a list
  of these tags.
