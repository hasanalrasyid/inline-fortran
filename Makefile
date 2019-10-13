default: clean
	stack build
	stack exec x
clean:
	stack clean
