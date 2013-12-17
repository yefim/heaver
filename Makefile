all:
	ghc -o heaver Main.hs

clean:
	rm *.o
	rm *.hi
	rm heaver
