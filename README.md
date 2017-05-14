# rdza

notes:
1) we have static typechecking
2) but can't check early returns

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
* runtime errors like zero division

