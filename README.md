# rdza

TODO

important:
* compilation with stack fails due to the lexer!

* now I started relops, but for that I need the boolean data type
* or maybe I don't need booleans just yet
* maybe function args as reader

* type parameter to the Interpreter monad specifies what Haskell type we get in the end
* we can't rely on deriving Ord; need to lift the operation explicitly

* why do we need the wrapper for values?
* suppose the program is invalid

eg ```EAdd (ELitInt 2) Plus ELitTrue```

then we evaluate to 2 + True

which is a Haskell type error

no need to rely on user input; the program itself may not typecheck in Haskell
