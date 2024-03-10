HC = ghc
HCFLAGS = -Wall

all: flp-fun

flp-fun: flp-fun.hs
	$(HC) $(HCFLAGS) -o flp-fun flp-fun.hs 

clean: 
	rm flp-fun flp-fun.o flp-fun.hi