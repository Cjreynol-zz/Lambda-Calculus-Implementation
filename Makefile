main:
	cabal exec -- ghc src/Main.hs -Wall -isrc -outputdir bin -o lambdaCalc

interactive:
	cabal exec -- ghci src/Examples.hs -isrc

documentation:
	cabal exec -- haddock -o docs -h src/*.hs

clean:
	rm lambdaCalc bin/*.hi bin/*.o

cleanDocs:
	rm -f docs/*

nuke:
	make clean; make cleanDocs

all:
	make main; make documentation
