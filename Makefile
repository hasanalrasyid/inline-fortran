default:
	stack build
	stack exec x
clean:
	stack clean
r: clean default
