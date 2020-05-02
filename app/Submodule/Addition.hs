module Submodule.Addition where

import Language.Fortran.Inline


extendContext basic
extendContext functions

setCrateRoot []

additionalFunction :: Double -> Double -> Double
additionalFunction x y = x * y * x

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