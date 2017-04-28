# rdza

TODO

* to start with, we just have one function main
* and one type int
* we need to have a nice monad with:
    * function args are like env in reader
    * runtime errors are reported to error monad (either)
    * statements like states
    * monad stack are easily extensible! start with state and exception
* to start with, main function with no args
* my guess would be that we'll have a chain of frames with env, corr. to function frames
* but store local vars in state perhaps
* remember we have expressions and statements
* let me interpret both expr and statements
* expr only need reader but no state
* statements need state
* don't worry about nested functions for now; compose Except and State / Reader for now