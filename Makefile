export HEREIN=$(shell pwd)
export INLINE_FORTRAN_FFLAGS = -g -J$(HEREIN)/exampleF95
default:
	stack --verbose build
	stack test
clean:
	stack clean inline-fortran
r: clean default
