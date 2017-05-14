# rdza

important:
* compilation with stack fails due to the lexer!

ask instructor:
1) how about immutable data types? then no distinction between passing
    by reference and value; how does this affect closures?
2) how are immutable dictionaries and maps implemented?

fix syntax:
* but the distinction makes sense because we don't want decls and ass in exprs
* but same applies to while-loops, so we do want to move them to stmts
* same goes for if without else
* don't require semicolon after blocks

well know algorithms:
* type reconstruction
* identifier scopes and shadowing




simple:
* when typechecking, all we need to do is that type of return agrees with the function return type
* when evaluating a function, catch returns




todo:
* impl return
* print function
* print function: to avoid IO in interpreter, add Writer monad to the stack
* pass params to main function?
* strings
* then shadowing and scoping
* then complex data type like lists, dicts, tuples

functions:
* each function inherits the scope it was defined in
    so for global functions, the scope contains top level functions
    for nested functions and anonymous functions, local scope

requirements:
* now on to functions
* 1) pass by value
* 2) pass by reference
* print
* optional: for loop
* string type
* ---
* identifier shadowing and scopes
* runtime errors like zero division
* functions returning values
* lists / maps / records
* tuples with destructuring
* pass function as parameter
* return function, function closures
* anonymous functions
* ---
* nested functions