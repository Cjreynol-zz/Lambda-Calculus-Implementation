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
term to be reduced.  Lambdas are represented by `\` characters and terms 
are written using standard De Bruijn syntax, where the variables are 
natural numbers that reference their binder.  
  
Application is implicit, but there are cases where the parser fails to 
recognize valid terms or improperly parses others.  To avoid this problem, 
fully parenthesize all of the terms you provide as input.  For example, 
`(\.00)\.0` is valid, but should be entered as `(\.00)(\.0)` to avoid any 
errors.  

After a term is entered, if it terminates a reduction sequence to its normal 
form will be displayed.  
