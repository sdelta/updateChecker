COMPILER=ghc
IDIR=sources
GENERIC_OPTIONS=-i$(IDIR) -Wall

all: 
	$(COMPILER) ${GENERIC_OPTIONS} --make Main -O2 -o updateChecker 
