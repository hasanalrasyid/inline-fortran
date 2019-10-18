export HEREIN=$(shell pwd)
export INLINE_FORTRAN_FFLAGS = -g -J$(HEREIN)/exampleF95
default:
	stack build
	stack exec i
clean:
	stack clean
r: clean default
