GHC_FLAGS := 

all:	build/main.out
.IGNORE: all

clean:
	rm -rf build/*.out
	rm	-rf *.hi
	rm	-rf *.o
.IGNORE: clean

build/%.out:	%.hs
	ghc $(GHC_FLAGS) -o $@ $^
