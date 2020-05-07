module Submodule.Addition where

import Language.Fortran.Inline
import Foreign
import Language.Fortran.Inline.Utils
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

extendContext basic
extendContext functions
extendContext pointers
extendContext fVectors
extendContext vectors

setCrateRoot []

additionalFunction :: Double -> Double -> Double
additionalFunction x y = x * y * x

aFun3 :: Ptr Double -> Double -> IO Double
aFun3 x y = do
  x' <- peek x
  return $ x' + 3 * y

aFun4 :: Double -> IO ()
aFun4 x = do
  putStrLn $ "inside aFun4: " ++ show (x * 12 :: Double)

aFun5 :: Ptr Double -> IO ()
aFun5 x1 = do
  x <- vectorFromC 5 x1
  putStrLn $ "inside aFun5: " ++ show x
    {-
       should be called from sumthing like
  u1 <- VM.replicate 5 2 :: IO (VM.IOVector Double)
  r <- unsafeWithVectors [u1] $ \(u:_) -> do
        outModule u
    -}
outModule :: Ptr Double -> IO Double
outModule u = do
  -- Fortran can only import IO a functions. By design, it cannot import pure function
  y <- [fortIO| real(kind=8) ::
      IMPLICIT NONE
      real(kind=8) :: f
      real(kind=8),target :: m(5)
      integer :: i
      do 22 i=1,5
  22    m(i) = i + i
      f = m(2) * 2
      call $func:(aFun5:():real(kind=8)*1)(c_loc(m))
      $return = f
    |]
  putStrLn $ "otherModule: " ++ show y
  return y

--    f = $func:(aFun3:real(kind=8):*real(kind=8):*real(kind=8):real(kind=8)) (m,$(x:value:real(kind=8)))

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
