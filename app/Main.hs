{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int
import Language.C.Inline
import Foreign
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

extendContext basic
extendContext vectors

setCrateRoot []



main = do
  putStrLn "Haskell: Hello. Enter a number:"
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

  let ix = 2
  let vInit = V.fromList $ take 9 [0,0 .. ] :: V.Vector Float
  let vInit1 = V.fromList $ take 9 [0,0 .. ] :: V.Vector Float
--  v <- V.thaw vm
  putStrLn $ "Haskell: says vInit: " ++ (show vInit)
  putStrLn $ "Haskell: says x: " ++ (show ix)
  V.unsafeWith vInit $ \v -> do
    xp <- withPtr $ \x -> do -- this is for output
      V.unsafeWith vInit1 $ \v1 -> do
        [fortIO|
! # C macro dideteksi di level haskell... unexpected... but OK or better
#if defined (CPP)
      use module3
#else
      use module1
#endif
      IMPLICIT iNTEGER (I-R)
      character :: c
      integer :: a,NAX

      dimension v1(9)

      print *, "adalah dianya yang "

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
      print *, "test v1",$vec(v1:inout:real)(1)
      print *, "adalah lagi ",k
      ix = 54
      print *, "adalah lagi ix ",ix
      do 300 j = 1,3
      do 301 i = 1,3
          $vec(v:inout:real:(3,3))(i,j) = l
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

-- Utils

vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
vectorToC vec len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec

