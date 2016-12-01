
# for compilation in the sandbox
main:
	cabal exec -- ghc src/main.hs -isrc -outputdir bin -o lambdaCalc

# for loading up an interactive session with the lambda calculus
interactive:
	cabal exec -- ghci src/examples.hs -isrc
