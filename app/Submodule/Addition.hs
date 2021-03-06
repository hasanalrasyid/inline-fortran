module Submodule.Addition where

import Language.Fortran.Inline
import Foreign
import Language.Fortran.Inline.Utils
import Eigen.Internal
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable as V
extendContext basic
extendContext functions
extendContext pointers
extendContext fVectors

setCrateRoot []

additionalFunction :: Double -> Double -> Double
additionalFunction x y = x * y * x

aFun3 :: Ptr Double -> Double -> IO Double
aFun3 x y = do
  x' <- peek x
  let r = x' + 3 * y
  putStrLn $ "aFun3: r: " ++show r
  return r

aFun4 :: Double -> IO ()
aFun4 x = do
  putStrLn $ "inside aFun4: " ++ show (x * 12 :: Double)

aFun5 :: Ptr Double -> Int -> IO ()
aFun5 x1 n = do
  x <- vectorFromC n x1
  putStrLn $ "inside aFun5: " ++ show x

aFun6 :: Ptr (CComplex Float) -> IO ()
aFun6 x1 = do
  x <- peek x1
  putStrLn $ "inside aFun6: " ++ show x
  poke x1 $ CComplex 6.5 4.3
  putStrLn $ "!aFun6"

aFun7 :: Ptr (CComplex Float) -> Ptr Double -> Int -> IO ()
aFun7 vcp _ n = do
{- This is a must: generate Vector using vectorFromC,
    then thaw it using unsafeThaw to edit the vectors
    then operate it using unsafeWithVectors, here we can use fortIO

    Maybe on the next occasion, we can automate this process.
    -}
  v0 <- vectorFromC n vcp
  v1 <- V.unsafeThaw v0
  putStrLn $ "aFun7: v vcp: " ++ show v0
  unsafeWithVectors [v1] $ \(v:_) -> do
    poke v (CComplex 6.6 2.2)

aFun8 :: IO ()
aFun8 = do
  putStrLn "aFun8: test for no params procedure"

    {-
       should be called from sumthing like
  u1 <- VM.replicate 5 2 :: IO (VM.IOVector Double)
  r <- unsafeWithVectors [u1] $ \(u:_) -> do
        outModule u
    -}
outModule :: Ptr Double -> IO Double
outModule u = do
  let xx = 23
  ua1 <- VM.replicate 3 (CComplex 1.2 2.3) :: IO (VM.IOVector (CComplex Float))
  (_,r) <- withPtr $ \(p:: Ptr Int) -> do
    (_,rC) <- withPtr $ \(cp :: Ptr (CComplex Float)) -> do
      unsafeWithVectors [ua1] $ \(vcp:_) -> do
        poke cp $ CComplex 2.3 4.5
  -- Fortran can only import IO a functions. By design, it cannot import pure function
        y <- [fortIO| real(kind=8) ::
      IMPLICIT NONE
      real(kind=8) :: f
      real(kind=8) :: m(5,3)
      integer :: i,j
      complex(kind=8) :: c
      c = cmplx(1.2,3.4)
      do 22 i=1,5
        do 22 j = 1,3
  22    m(i,j) = i +  (j* 0.10)
      f = m(2,2) * 2
      print *,'fortIO: outModule: u:',$vec(u:inout:real(kind=8):1)(1)
      print *,'fortIO: outModule: xx: ',$(xx:value:integer)
      f = $(p:inout:integer)
      call $proc(aFun5:():*real(kind=8):integer)(m,15)
      f = $proc(aFun3:real(kind=8):*real(kind=8):real(kind=8)) (m,m(3,2))
      do 33 i=1,15
        print *,'outModule: u: ',u(i)
        u(i) = 10*i
  33  continue
      c = $(cp:inout:complex(kind=8))
      print *,'outModule: c: ',c
      cp = complex(6.7,8.9)
      print *,'try vec of complex: ', $vec(vcp:inout:complex(kind=8):1) (1)
      vcp(2) = (4.5,6.7)
      call $proc(aFun6:():complex(kind=8))(cp)
c     Due to howt complex marshaled to CComplex, we cannot make a function that
c     have return value of complex
c     as a workaround, we should change
c     res = aFun7(params)
c     into
c     call afun7(params,res) with complex :: res
c     c = $proc(aFun7:complex(kind=8):complex(kind=8):*real(kind=8):integer)(cp,m,2)
      call $proc(aFun7:():*complex(kind=8):*real(kind=8):integer)(vcp,m,3)
      call $proc(zeroc:():*complex(kind=8):integer) (vcp,3)
      call $proc(aFun8:())
      print *,'outModule: c: ',cp
      $return = f
      |]
        putStrLn $ "otherModule: " ++ show y
        cpx <- peek cp
        putStrLn $ "otherModule: *cp: " ++ show cpx
        return y
    return rC
  ua1Frozen <- V.unsafeFreeze ua1
  putStrLn $ "otherModule: ua1 vcp frozen: " ++ show ua1Frozen
  return r

zeroc :: Ptr (CComplex Float) -> Int -> IO ()
zeroc x n = do
  putStrLn $ "zeroc: "
  [fortIO| () ::
c     SUBROUTINE ZEROD(X,N)
C     CopyRight T.Oda (2003.12.31)
      IMPLICIT NONE
      integer :: I

#if defined(__PARA_OMP_ZERO)
!$    USE OMP_LIB
      USE PARA_MPI, ONLY: NUM_OMP
#endif
c     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c     INTEGER N,I
c     REAL*8 X(N)
C
c     IF($(N.LE.500)GOTO 200
      IF($(n:value:integer).LE.500)GOTO 200
C
#if defined(__PARA_OMP_ZERO)
!$OMP PARALLEL
!$OMP& PRIVATE(I,ITHR,J1,J2)
      ITHR=OMP_GET_THREAD_NUM()
      CALL SETJ1J2(N,NUM_OMP,ITHR,J1,J2)
      DO I=J1,J2
        X(I)=0
      ENDDO
!$OMP END PARALLEL
      RETURN
#endif
C
  200 CONTINUE
C
      DO I=1,N
c       X(I)=0d0
        $vec(x:inout:complex(kind=8):1)(I)=0
      ENDDO
C
      RETURN
c     END
  |]

zerod :: Ptr Double -> Int -> IO ()
zerod x n = do
  putStrLn $ "zerod: "
  [fortIO| () ::
c     SUBROUTINE ZEROD(X,N)
C     CopyRight T.Oda (2003.12.31)
      IMPLICIT NONE
      integer :: I

#if defined(__PARA_OMP_ZERO)
!$    USE OMP_LIB
      USE PARA_MPI, ONLY: NUM_OMP
#endif
c     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
c     INTEGER N,I
c     REAL*8 X(N)
C
c     IF($(N.LE.500)GOTO 100
      IF($(n:value:integer).LE.500)GOTO 100
C
#if defined(__PARA_OMP_ZERO)
!$OMP PARALLEL
!$OMP& PRIVATE(I,ITHR,J1,J2)
      ITHR=OMP_GET_THREAD_NUM()
      CALL SETJ1J2(N,NUM_OMP,ITHR,J1,J2)
      DO I=J1,J2
        X(I)=0d0
      ENDDO
!$OMP END PARALLEL
      RETURN
#endif
C
  100 CONTINUE
C
      DO I=1,N
c       X(I)=0d0
        $vec(x:inout:real(kind=8):1)(I)=0d0
      ENDDO
C
      RETURN
c     END
  |]
