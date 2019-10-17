export INLINE_FORTRAN_CC = -g
default:
	stack test
	stack exec x
clean:
	stack clean
r: clean default
