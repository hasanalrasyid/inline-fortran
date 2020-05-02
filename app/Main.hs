{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Language.Fortran.Inline
--import Language.Fortran.Inline.Utils
--import Data.Int
import qualified Language.C.Inline as C
import Foreign
import Foreign.C.Types
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
--import qualified Data.Text.Foreign as T
--import qualified Data.Text as T
--import Eigen.Internal

import External

--import           Text.RawString.QQ (r)

$(genWithPtrs 20)

--extendContext vectors
extendContext fVectors
extendContext functions
extendContext basic

setCrateRoot []

C.context (C.baseCtx <> C.funCtx <> C.fptrCtx)

main :: IO ()
main = do
  putStrLn "Haskell: Hello. Enter a number:"
    {-
  let angstr  = 1/0.5291769
  let celvin  = 1/(2*13.606*11605)
  let second  = 1.0+17/2.4189
  let eV      = 1/27.212
  let rY      = 0.5
  let tera    = 1.0e+12
  let pico    = 1.0e-12
  let scMass  = 1822.89
  let factem  = 2*13.606*11605
  let gpa     = 1/29.4215e3
  let fpi     = 4*pi

  let nax = 3
  let ix = 2
  let sInit' = "this is the string"
  let sInit = T.append sInit' $ T.replicate (256 - T.length sInit') "X"
  let vInit = V.fromList $ take 9 [0,0 .. ] :: V.Vector Float
  let vInit1 = V.fromList $ take 9 [0,0 .. ] :: V.Vector Float
  let vInitCComplex = V.fromList $ take 9 $ repeat (CComplex 0 0) :: V.Vector (CComplex Double)
  putStrLn $ "Haskell: says vInitCComplex: " ++ show vInitCComplex
--  v <- V.thaw vm
  putStrLn $ "Haskell: says vInit: " ++ (show vInit)
  putStrLn $ "Haskell: says x: " ++ (show ix)
  V.unsafeWith vInit $ \v -> do
    xp <- withPtr $ \x -> do -- this is for output
      V.unsafeWith vInit1 $ \v1 -> do
        T.withCStringLen sInit $ \(sInit,_) -> do
          [fortIO| () ::
! # C macro dideteksi di level haskell... unexpected... but OK or better
#if defined (CPP)
      use module3
#else
      use module1
#endif
      IMPLICIT iNTEGER (I-R)
      character :: c
      integer :: a
      character*256 :: sInit

      dimension v1(9)
      dimension v(nax,nax)

      print *, "sInit ", $str(sInit:in:18)
      print *, "adalah dianya yang "

      $(nax :value:integer) = 3
      $(angstr :value:real(kind=8)) = angstr *1
      $(celvin :value:real(kind=8)) = celvin *1
      $(second :value:real(kind=8)) = second *1
      $(eV     :value:real(kind=8)) = eV *1
      $(rY     :value:real(kind=8)) = rY *1
      $(tera   :value:real(kind=8)) = tera *1
      $(pico   :value:real(kind=8)) = pico *1
      $(scMass :value:real(kind=8)) = scMass *1
      $(factem :value:real(kind=8)) = factem *1
      $(gpa    :value:real(kind=8)) = gpa *1
      $(pi     :value:real(kind=8)) = pi *1
      $(fpi    :value:real(kind=8)) = fpi *1

C-Testing for comment  1
C Testing for comment  1
! Testing for comment  2
c Testing for comment  3
      NAX = 3
      do j = 1,3
      do i = 1,3
          print*,"test v:",i,j, v(i,j)
        end do
      end do
      k = 1
      # 373 test
10001   FORMAT('IDLING TIME : ',I10,' sec (',F6.2,' %)')
      print *, "adalah ",k
      l = 1
      k = 1 + $(ix:value:real(kind=8))
c     print *, "test v1",$vec(v1:inout:real:1)(1)
      print *, "adalah lagi ",k
      ix = 54
      print *, "adalah lagi ix ",ix
      do 300 j = 1,3
      do 301 i = 1,3
          $vec(v:inout:real:0)(i,j) = l
          l = l + 1
  301   continue
  300 continue
      print *, "adalah dianya yang "
      $(x:out:real) = x + 5
      write(*,10001) k,
     & x
      do 400 j = 1,3
      do 401 i = 1,3
          print*,"test v:",i,j, v(i,j)
  401   continue
  400 continue
 3610         FORMAT(' NFI=',I6,4(1X,F9.4))
 5640     FORMAT(3F15.9)
          |]
          -- k = 5 + $(x : i32) # anehnya, ini error
      putStrLn $ "Haskell: Rust says in withPtr v=" ++ show v
      xContent <- peek x
      putStrLn $ "Haskell: Rust says in withPtr x=" ++ show xContent
    putStrLn $ "Haskell: says v changed   : " ++ (show xp)
  putStrLn $ "Haskell: says v changed    : " ++ (show vInit)
  putStrLn $ "Haskell: says ix unchanged : " ++ (show ix)
  hSep ""
  ua1 <- VM.replicate 2 1 :: IO (VM.IOVector Double)
  ua2 <- VM.replicate 2 2 :: IO (VM.IOVector Double)
  ua3 <- VM.replicate 2 3 :: IO (VM.IOVector Double)
  ua4 <- VM.replicate 2 4 :: IO (VM.IOVector Double)
  let uas = [ua1,ua2,ua3,ua4]
  unsafeWithVectors uas $ \(u1:u2:u3:u4:_) -> do
--  (flip mapM_) l $ \p -> do
--    t <- peek p
--    poke p $ t * t
--    tt <- peek p :: IO Double
--    putStrLn $ show tt
    [fortIO| () ::
      IMPLICIT NONE
      integer :: i
      dimension u1(2)
      $vec(u1:inout:real(kind=8):0)(1) = 3
      do 300 i=1,2
        u1(i) = 33*i
  300 continue
      print*,"test u1:",u1

    |]
  uasFrozen <- mapM V.unsafeFreeze uas
  putStrLn $ "uas: " ++ show uasFrozen
  hSep ""
--a <- mapM V.unsafeFreeze [ua1,ua2,ua3,ua4]
--putStrLn $ "a:" ++ show a
  b <- withPtrs3 $ \l -> do -- \l@(a1:a2:a3:[]) -> do
    (flip mapM_) l $ \p -> do
      t <- peek p
      poke p $ 2 * t
      tt <- peek p :: IO Int
      putStrLn $ show tt
  putStrLn $ show b
-- Utils
  hSep ""
  splitF90 "test/f90split_test.f90"
--hSep ""
--test2
  hSep ""
  test3
---}
  hSep ""
  test4
  hSep ""
  test5

--withPtrs3 :: (V.Storable a) => ([Ptr a] -> IO ()) -> IO [a]
--withPtrs3 = $(withPtrsN 3)

test4 :: IO ()
test4 = do
  putStrLn "===== test4"
  let pureFuncIO x = return $ pureFunc x
  x <- withPtr $ \pp -> do
        poke pp 2.3
        p <- newForeignPtr_ pp :: IO (ForeignPtr CDouble)
        y <- [C.block| double
          {
            double (*f)(double);
            double *pc;
            pc = $fptr-ptr:(double *p);
            return $fun:(double (*pureFuncIO) (double))(*pc);

          }
        |]
        return (y :: CDouble)
  putStrLn $ "==== x: " ++ show x
  putStrLn $ "=====!test4 " -- ++ (show x)

theFun :: Double -> IO Double
theFun x = return $ x*x + 1

test5 :: IO ()
test5 = do
  putStrLn $ "test5: ==================="
  let theFun2 x = return $ x + x * 2
  let x = 2.3
  y <- [fortIO| real(kind=8) ::
          IMPLICIT NONE
          real(kind=8) :: f
          print *,"test this: ", x
          f = $func:(theFun2:real(kind=8):real(kind=8))($(x:value:real(kind=8)))
          $return = f
       |]
  putStrLn $ "===: y: " ++ show y
  putStrLn $ "test5: try for withFunPtr "

hSep :: String -> IO ()
hSep s = putStrLn $ take 70 $ "===" ++ s ++ (repeat '=')

vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
vectorToC vec len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec

test2 :: IO ()
test2 = do
  putStrLn "====test2"
  (nax,x) <- withPtr $ \nax -> do
    poke nax 888
    x <-  [fortIO| real(kind = 8) ::
      real(kind = 8) :: d1
      real(kind = 8) :: dr

      d1 = 3.4
      print *,'this is testing'
      print *,'nax :',$(nax:inout:real(kind=8))
      nax = 777
      d1 = 3.7
c     dr = inline_c_Main_0(d1)
      print *,'d1:',d1
      print *,'dr:',dr
      $return = d1

          |]
    return x
  putStrLn $ "===!test2"
  putStrLn $ "nax: " ++ show (nax :: Double)
  putStrLn $ "x: " ++ show (x :: Double)

