COMPILER=ghc
GENERIC_OPTIONS='-Wall'

all: Main.hs FileInteraction.hs PrintMatrix.hs HTML.hs
	ghc ${GENERIC_OPTIONS} --make Main -O2 -o updateChecker 
