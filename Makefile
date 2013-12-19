all:
	ghc -o heaver Main.hs

test:
	make
	./tests/tests.sh

clean:
	rm *.o
	rm *.hi
	rm heaver
