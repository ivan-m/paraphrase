* Make it _fast_!

* Improve quality of pretty-printing (colours?)

    - Limit length of inputs stored in error messages.  This will
      improve readability in displays and also memory usage of large
      logs.

* Add more inbuilt types to PrettyValue (at the very least all the
  numeric types in the Prelude)

* Auto-streaming parsers (e.g. convert lazy Bytestring to a list of
  strict ones and parse those).

* endOfInput doesn't take into account getting more

* Consider having a "stackTraceOnFailure" function to only add in a
  stack trace when it fails.  This should reduce useless noise from
  parsers that succeed.  Not sure how well it will work with other
  combinators that manipulate the Failure function.

* Log verbosity?

* Do we need a distinction between commit and commitNoLog?  Should
  many and some still use commitment?

* Have a reason why something is committed

* A function to do "commit squashing" in error messages so you only
  see the final Committed error that applies.
