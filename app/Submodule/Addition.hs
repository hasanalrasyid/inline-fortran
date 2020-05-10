module Submodule.Addition where

import Language.Fortran.Inline
import Foreign
import Language.Fortran.Inline.Utils
import Eigen.Internal

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

aFun6 :: Ptr (CComplex Double) -> IO ()
aFun6 x1 = do
  x <- peek x1
  putStrLn $ "inside aFun6: " ++ show x

    {-
       should be called from sumthing like
  u1 <- VM.replicate 5 2 :: IO (VM.IOVector Double)
  r <- unsafeWithVectors [u1] $ \(u:_) -> do
        outModule u
    -}
outModule :: Ptr Double -> IO Double
outModule u = do
  let xx = 23
  (_,r) <- withPtr $ \(p:: Ptr Int) -> do
    (_,rC) <- withPtr $ \(cp :: Ptr (CComplex Double)) -> do
      poke cp $ CComplex 2.3 4.5
  -- Fortran can only import IO a functions. By design, it cannot import pure function
      y <- [fortIO| real(kind=8) ::
      IMPLICIT NONE
      real(kind=8) :: f
      real(kind=8) :: m(5,3)
      integer :: i,j
      complex(kind=8) :: c
      c = cmplx(1.2,3.4)
c     call $proc:(aFun6:():complex(kind=8))(c)
      do 22 i=1,5
        do 22 j = 1,3
  22    m(i,j) = i +  (j* 0.10)
      f = m(2,2) * 2
      print *,'fortIO: outModule: u:',$vec(u:inout:real(kind=8):1)(1)
      print *,'fortIO: outModule: xx: ',$(xx:value:integer)
      f = $(p:inout:integer)
      call $proc:(aFun5:():*real(kind=8):integer)(m,15)
      f = $proc:(aFun3:real(kind=8):*real(kind=8):real(kind=8)) (m,m(3,2))
      do 33 i=1,15
        print *,'outModule: u: ',u(i)
        u(i) = 10*i
  33  continue
      c = $(cp:inout:complex(kind=8))
      print *,'outModule: c: ',c
      cp = complex(6.7,8.9)
      $return = f
      |]
      putStrLn $ "otherModule: " ++ show y
      putStrLn $ "otherModule: cp: " ++ show cp
      return y
    return rC
  return r


  {-
otherModule :: Double -> IO Double
otherModule x = do
  let additionalFunctionIO a b = return $ additionalFunction a b
  -- Fortran can only import IO a functions. By design, it cannot import pure function
  y <- [fortIO| real(kind=8) ::
      IMPLICIT NONE
      real(kind=8) :: f
      real(kind=8) :: m
      m = 3
      f = $func:(additionalFunctionIO:real(kind=8):real(kind=8):real(kind=8)) (m,$(x:value:real(kind=8)))
      $return = f
    |]
  putStrLn $ "otherModule: " ++ show y
  return y
-}
