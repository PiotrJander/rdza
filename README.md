# rdza

important:
* compilation with stack fails due to the lexer!

ask instructor:
1) how about immutable data types? then no distinction between passing
    by reference and value
2) how are immutable dictionaries and maps implemented?

fix syntax:
* but the distinction makes sense because we don't want decls and ass in exprs
* but same applies to while-loops, so we do want to move them to stmts
* same goes for if without else
* don't require semicolon after blocks

plan for today:
* implement functions, then go to sleep

ideas:

* by convention, we start with the main function
* function is an data type which can be executed
* top level function belong to global scope
* if we have a Value constructor function, then we can have anonymous
    functions, return functions, take functions as arguments etc
* but we might need elaborate function types for that
* closures: like in PHP, when we make anonymous function, give
    names of identifiers to store in the closure env (optional, later)
* recursion should be straightforward
* nested function are just named closures
* maybe function args as reader, then can't be overriden
* but can be shadowed with local identifiers
* for now assume immutable data types and pass by value only

* passing numbers as reference
* can we have two references to the same value, or the same value by
* the same name?
* then we'd need a global "memory" which is mapping from "addresses" to values
* and scopes contain mappings from names to "addresses"
* address could be simply unique number, counter
* we could also have purely immutable data types
* which, given that rdza data types translate to haskell types, makes sense

* print function: to avoid IO in interpreter, add Writer monad to the stack
* add the print keyword
* might need string type for that; but we already have string literals
* maybe add a few functions on strings
* like in Haskell, string might be a list

* also implement the return mechanism!
* idea: return value as Left error, might be caught at the end of the function

* then shadowing and scoping
* then complex data type like lists, dicts, tuples

thoughts:
* speaking of closures, we could have a "pointer to the frame of parent"
* this begs the question of references and mutability
* to mutate an object is to replace it with another object,
    but keep address the same
* in C to pass by value means to copy the value to the child frame,
    (copy gets a new address)
    and to pass by reference means to pass its address
* if we have memory, we might need garbage collection

requirements:
* now on to functions
* 1) pass by value
* 2) pass by reference
* must support recursion!
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