
# cabal exec commands used to play nicely with cabal sandboxes
main:
	cabal exec -- ghc src/main.hs -isrc -outputdir bin -o lambdaCalc

interactive:
	cabal exec -- ghci src/examples.hs -isrc

pdf:
	cd out; make

clean:
	cd bin; rm *.hi *.o; cd ../out; make clean
