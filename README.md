# Lambda Calculus  
This project is an implementation of untyped lambda calculus with De Bruijn 
indices.  
  
## Build  
This project has most recently been tested using GHC 8.2.2, and requires an 
installation of the `parsec` library.  See the included `.cabal` file for 
specific version requirements.

From the root directory, run `cabal build` to install dependencies and build 
the executable, which will be located in the dist/build/hask-lambda 
directory.  

## Run  
Running the lambdaCalc executable will bring up a prompt to enter in a lambda 
term to be reduced.  From the parser documentation:
  
> No spaces, '\\.' to represent lambda binders.  Vars are represented as 
> natural numbers referencing their binder.  That binder could be implicit 
> with free variables in the terms.  Application is implicit with adjacent 
> terms, and parenthesis can be used to end the binding scope of a lambda.  
> 
> For example:  
> 
> `\.00` is equivalent to `(\.(0 0))` while `(\.0)0` is equivalent to 
> `(\.0) 0)`, which is a beta redex.

After a term is entered, if it terminates a reduction sequence to its normal 
form will be displayed.  Non-terminating terms cause an infinite loop
