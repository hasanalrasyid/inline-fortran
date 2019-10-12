default:
	rm -rf .stack-work/tmpdir/*
	rm -rf .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/x/
	stack build
	more .stack-work/g*/ghc*f95

