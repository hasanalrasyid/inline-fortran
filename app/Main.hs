{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Fortran.Inline
import Data.Int

extendContext basic

setCrateRoot []

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  x <- readLn
  y <- [rustIO| i32 {
C--------1---------2---------3---------4---------5---------6---------7---------8
      COMPLEX*16 FUNCTION XCSUM( N, DX, INCX )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C--------1---------2---------3---------4---------5---------6---------7---------8
C     DOUBLE PRECISION FUNCTION DSUM ( N, DX, INCX )
C     SUM OF THE ELEMENTS OF A VECTOR.
C     USES UNROLLED LOOPS FOR INCREMENT EQUAL TO ONE.
C     JACK DONGARRA, LINPACK, 3/11/78. CJB
C-------------------------------------------------------------------------------
C     DSUM WAS MODIFIED TO XCSUM BY T.Oda(1998.01.21), WHICH CALCULATE
C     SUM OF THE ELEMENT OF A COMPLEX VECTOR.
C-------------------------------------------------------------------------------
C
      COMPLEX*16 DX(*)
      INTEGER I,INCX,N,NINCX,LINCX
C
      XCSUM = (0.0D0,0.0D0)
      IF(N.LE.0)RETURN
C
      NINCX = N*INCX
      LINCX = INCX + 1
      XCSUM = DX(1)
      DO 10 I = LINCX,NINCX,INCX
        XCSUM = XCSUM + DX(I)
   10 CONTINUE
      RETURN
      END
  } |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y
