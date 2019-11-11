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
  let ix = 2
  let vInit = V.fromList $ take 9 [0,0 .. ] :: V.Vector Float
--  v <- V.thaw vm
  putStrLn $ "Haskell: says vInit: " ++ (show vInit)
  putStrLn $ "Haskell: says x: " ++ (show ix)
  V.unsafeWith vInit $ \v -> do
    xp <- withPtr $ \x -> do -- this is for output
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
      print *, "adalah dianya yang "
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

