SRC=$(shell ls src/*.hs)
TARGET=./dist/build/hBugs/hBugs

.PHONY: all versions bugs help version fix-build clean pack

all $(TARGET): $(SRC)
	./Setup.lhs configure --user
	./Setup.lhs build

versions bugs:
	 $(TARGET) -l maxima_lib/ $@

help version:
	 $(TARGET) --$@

clean:
	cabal clean
	rm -f *.wxm

fix-build:
	cabal update
	cabal install parsec
	cabal install regex-compat

pack:
	tar cjvf ../hBugs.tbz2 -C .. --exclude-vcs --exclude dist --exclude *~ hBugs 
