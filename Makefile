export INLINE_FORTRAN_CC = -g
default:
	stack build
	stack exec x
clean:
	stack clean
r: clean default
