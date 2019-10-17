export INLINE_FORTRAN_CC = -g
default:
	stack build
	stack exec i
clean:
	stack clean
r: clean default
