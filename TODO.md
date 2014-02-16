* Make it _fast_!

* Should the `ParseError` type really have constructors that store the
  input?  After all, the input will be available at the end of the
  parse...

    - Then again, if it's mid-parse and you want the result, this will
      provide it.

    - In which case, more of them should have the input at that point.

* Better pretty-printing function for ParseLog output.

* Deal with errors when chaining parsers

    - A GADT maybe?

* Auto-streaming parsers (e.g. convert lazy Bytestring to a list of
  strict ones and parse those).
