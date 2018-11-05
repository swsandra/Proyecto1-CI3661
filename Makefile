all: sabio

sabio :
	ghc -o sabio --make -main-is main Sabio.hs
	haddock -h Sabio.hs -o ./Doc
	rm -f *.o *.hi *~

clean :
	rm -f sabio *.o *.hi *~