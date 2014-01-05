% How this library differs from `polyparse`
% Ivan Miljenovic

For the purposes of this document/library, the "`[polyparse]`" parsers
are those defined in the `Text.ParserCombinators.Poly.*` modules of
that library.  Specifically, for versions of polyparse >= 1.9 (due to
the change in implementation of the `bracket` combinator).

As a re-imagined polyparse-based parsing combinator library, the API
and method of usage as a whole has been designed to reflect that of
the various polyparse parsers.  However, there are some differences
which this document attempts to catalogue:

1. The types are obviously different.  `polyparse-next` is polymorphic
   in terms of the input rather than a separate parser type for every
   possible input type.

    - This means now that the various parsing combinators are all
      polymorphic rather than on a per-parser case.  This means that
      (for example) you're not relying on the convention that `next`
      is defined in each parsing module (as its type could not be
      specified on a class basis).

    - As a consequence of this, you may need to provide explicit
      type-signatures for parsers.

2. The implementation is based upon `[attoparsec]` and as such is
   CPS-based rather than constantly comparing against a `Result`
   value.  This may some implications in terms of how some combinators
   work (but no large differences are currently known).

    - It is also unknown whether the `commit` combinator helps prevent
      memory leaks, etc. like it does in polyparse (it does prevent
      backtracking though).

3. A larger focus on using existing functions/operators:

    - Combinators like `many1`, `discard` and `onFail` are no longer
      exported as they are identical to existing class methods (in
      this case `some`, `(<*)` and `(<|>)` respectively).

    - `many1Finally` is known as `someFinally` (in line with `some`
      instead of `many1`) and is now available for all input types.

    - The exception to this is that `chainParsers` is re-exported
      despite being identical to `(.)` and `(<<<)` from
      `[Control.Category]`; this is primarily for documentation
      purposes (but also in case people prefer not to use the
      `Category` instance).

4. More in-built support for creating a "stack-trace" of error
   messages for when parsers fail.

5. No in-built support for stateful operations; instead, will use the
   `StateT` transformer from the `[transformers]` package (exact
   implementation to be determined).

6. The constructor for the parser isn't exported (this is subject to
   change, if it's found to be useful to be able to construct a parser
   manually).

7. The `bracketSep` function is implemented slightly differently; this
   will only have an impact on the possible error messages returned if
   parsing fails.

[polyparse]: http://hackage.haskell.org/package/polyparse

[attoparsec]: http://hackage.haskell.org/package/attoparsec

[transformers]: http://hackage.haskell.org/package/transformers

[Control.Category]: http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Category.html
