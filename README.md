# rdza

important:
* compilation with stack fails due to the lexer!

fix syntax:
* but the distinction makes sense because we don't want decls and ass in exprs
* but same applies to while-loops, so we do want to move them to stmts
* same goes for if without else
* don't require semicolon after blocks

ideas:
* maybe function args as reader
* when type checking if without else, require void type
* reorganize tests, give it structure, then run with patterns

now:
* ok static type checking
* program is an ast
* each expression is depends in some way on the type on its args
* we start at leaves, and fold the ast with most general type
* interpreter: do type check monad, then do interpreter monad

* then try parsing and interpreting a program
* then do basic static type checking
* then finish for today
