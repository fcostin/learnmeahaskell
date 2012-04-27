GHC_FLAGS := -threaded -Wall -O2 -rtsopts -prof

all:	build/main.out
.IGNORE: all

clean:
	rm -rf build/*.out
	rm	-rf *.hi
	rm	-rf *.o
	rm -rf Game/*.hi
	rm -rf Game/*.o
.IGNORE: clean

build/%.out:	%.hs
	ghc $(GHC_FLAGS) -o $@ $^
