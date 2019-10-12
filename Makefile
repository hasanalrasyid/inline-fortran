default:
	rm -rf .stack-work/tmpdir/*
	stack build
	more .stack-work/g*/ghc*f95

