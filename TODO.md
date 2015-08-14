* Make it _fast_!

* Improve quality of pretty-printing errors:

    - Colours?

    - Limit length of inputs stored in error messages.  This will
      improve readability in displays and also memory usage of large
      logs.

    - Print intial/final inputs?

    - Not all errors require inputs being stored; e.g. all the ones
      that take a log (as the internal log will store the input).

    - Get rid of unnecessary backtracking/sub-log errors?  Just
      because it backtracked right at the beginning doesn't mean we
      need to know that _now_... can probably re-use the
      commit-squashing machinery for this.

* Add more inbuilt types to PrettyValue (at the very least all the
  numeric types in the Prelude)

    - Then again, are people likely to want to use these?

    - What about using OverloadedInstances and having a default
      Show-based instance, so that if anyone wants a better
      representation they can write one?

* Expand upon chaining:

    - Need a "chainMany" that keeps running the source parser as it
      needs input (this can then be used for lazy parsers)

    - How can we provide a better error location for chained errors?

        + Have informational messages with initial feeding and
          additional inputs?

        + Overall error is currently with whatever input has not been
          fed to chained parser...

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

    - Get current input stream (for internal use only).

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

* Documentation:

    - Update ReadMe, Differences, etc.

    - Ensure internal comments are up-to-date and flesh them out.

    - Add `.Doc.*` "modules" with quick-start guides, comparisons to
      other parsing libraries, etc.

* Is it possible to convert the error type of an actual parser?

* Should we dump oneOf in favour of oneOf'?

    - Can oneOf be improved to avoid chained Backtrack errors?

* Should errors like ListWithTerminator take the resulting error from
  what they wrap?

    - Generalise this: have a variant of addStackTrace which wraps up
      a sublog.

    - Shoule ParseError be split up?  Have some kind of
      ParseErrorLabel type with unary constructors, then ParseError
      can have one that takes one of these Labels as well as a sublog.
