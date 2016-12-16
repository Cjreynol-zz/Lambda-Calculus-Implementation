
# if the Parsec library is installed in a sandbox, use the commented
# versions of the commands

main:
	#cabal exec -- ghc src/main.hs -isrc -outputdir bin -o lambdaCalc
	ghc src/main.hs -isrc -outputdir bin -o lambdaCalc

interactive:
	#cabal exec -- ghci src/examples.hs -isrc
	ghci src/examples.hs -isrc

pdf:
	cd out; make

clean:
	cd bin; rm *.hi *.o; cd ../out; make clean

nuke:
	make clean; rm lambdaCalc; cd out; make nuke
