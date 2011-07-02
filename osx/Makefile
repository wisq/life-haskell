GHC_OPTS=-O2 -threaded -rtsopts -Wall -fno-warn-unused-do-bind

test: wrapper.o SDLWrapper.hs
	ghc -no-hs-main $(GHC_OPTS) --make wrapper.o SDLWrapper.hs -o test
SDLWrapper_stub.h: SDLWrapper.hs
	ghc -no-hs-main $(GHC_OPTS) --make SDLWrapper.hs -c
wrapper.o: SDLWrapper_stub.h
	ghc -no-hs-main $(GHC_OPTS) `sdl-config --cflags` -Wall wrapper.c -c
clean:
	rm -f *.hi *.o *_stub.c *_stub.h test
.PHONY: clean

