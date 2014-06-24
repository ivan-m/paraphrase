* Make it _fast_!

* Improve quality of pretty-printing errors:

    - Colours?

    - Limit length of inputs stored in error messages.  This will
      improve readability in displays and also memory usage of large
      logs.

    - Log verbosity?

        + Have Backtrack, etc. store a ParseLog so that only the last
          error can be displayed.

        + Currently only raw inputs are (optionally) displayed; need to
          configure whether to also show any additional information from
          input wrapper types (e.g. Char8-based view when using AsChar8).

    - Commit-squashing: only show the last commitment rather than
      peppering them everywhere (with this, it might be feasible to
      just have many and some commit all the time).

* Add more inbuilt types to PrettyValue (at the very least all the
  numeric types in the Prelude)

    - Then again, are people likely to want to use these?

    - What about using OverloadedInstances and having a default
      Show-based instance, so that if anyone wants a better
      representation they can write one?

* Expand upon chaining:

    - Need a "chainMany" that keeps running the source parser as it
      needs input (this can then be used for lazy parsers)

* More wrapper inputs:

    - Count consumed tokens (so you know how far you've gone in the
      input).

    - Auto-streaming parsers (e.g. convert lazy Bytestring to a list
      of strict ones and parse those).

    - Is there any way we can automatically get classes like
      `Word8Input` applied to new wrappers?

* Expand/improve upon the input/stream distinction:

    - Currently you can't just easily write a parser inline in ghci,
      it needs a type (as input -> stream is injective, not bijective)

* Fix and add more parsers:

    - endOfInput doesn't take into account getting more

    - Have one to match an entire Stream

        + Requires adding `splitAt :: (ParseInput s) => Int -> s ->
          (Stream s, s)` and `length :: (ParseInput s) => Stream s ->
          Int` methods to the typeclass.

        + How to deal with wrapper types?  Do we match a Stream or the
          actual type (using `fromStream`?).  The latter would
          probably be preferable for `AsChar8`...

* Commitment:

    - Do we need a distinction between commit and commitNoLog?  Should
      many and some still use commitment?

    - Have a (optional?) reason why something is committed

* Should we have a separate module for extra Char-based parsers?

    - Export `AsWord8` there.

    - `IsString` instance.

    - Caseless variants of `token`, etc.

* Lifted class-based implementation, so you can have Reader, State,
  etc. wrappers.

* Need a test-suite

* Expand the benchmarks

    - Also benchmark different implementations of various combinators
      (e.g. next)
