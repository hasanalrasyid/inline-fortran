# inline-fortran

environment variables:
INLINE_FORTRAN_FFLAGS "-fpic -fno-underscoring"
INLINE_FORTRAN_FC "gfortran"

Features:
  [fortIO| FORTRAN blocks |]
  passed by value $(x:value:real)
  pass by reference, using Ptr, with intent $(y:inout:real)
  vector/array, with dimensions $vec(v:inout:real:(3,3))

in case of array:
we have to only consider single dimension array passing to each another function using
dimension (\*).
The question remains, can the dimension be recorded as another parameters?
it should be... using reshape or dimension

Todo:
- running haskell subroutine inside fortran,
    CALL $fun(procHaskell(p1,p2,p3,..))
  requirements:
    procHaskell :: a -> b -> .. -> IO () {- should be a subroutine -}
- sort the variable declaration for optimization

