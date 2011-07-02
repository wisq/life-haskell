GHC_OPTS=-O2 -threaded -rtsopts -Wall -fno-warn-unused-do-bind

all: test

clean:
	rm -rf glider-gun *.hi *.o

test:
	echo main | ghci test.hs > /dev/null

.test-stamp: Life.hs test.hs
	$(MAKE) test
	touch $@

glider-gun: glider-gun.hs Life.hs .test-stamp
	ghc $(GHC_OPTS) --make $<
