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
  let i = 2
  let vm = V.fromList $ take 9 [0,0 .. ] :: V.Vector Float
  v <- V.thaw vm
  putStrLn $ "Haskell: says vInit: " ++ (show v)
  putStrLn $ "Haskell: says x: " ++ (show i)
--  _  <- V.unsafeWith vInit $ \v -> do
--    rx <- withPtr_ $ \x -> do
  [fort77IO|
! # C macro dideteksi di level haskell... unexpected... but OK or better
#if defined (CPP)
      use module3
#else
      use module1
#endif
      IMPLICIT iNTEGER (I-R)
      character :: c
      integer :: a
      print *, "adalah dianya yang "
C-Testing for comment  1
C Testing for comment  1
! Testing for comment  2
c Testing for comment  3
      do j = 1,3
      do i = 1,3
          print*,"test v:",i,j, v(i,j)
        end do
      end do
      k = 1
      # 373 test
      $(x:out:real) = x + 5
      write(*,10001) k,
     & x
10001   FORMAT('IDLING TIME : ',I10,' sec (',F6.2,' %)')
      print *, "adalah ",k
      l = 1
      do 300 j = 1,3
      do 301 i = 1,3
          $vec(v:inout:real:(3,3))(i,j) = l
          l = l + 1
  301   continue
  300 continue
      print *, "adalah dianya yang "
      do 400 j = 1,3
      do 401 i = 1,3
          print*,"test v:",i,j, v(i,j)
  401   continue
  400 continue

  |]
      -- k = 5 + $(x : i32) # anehnya, ini error
--      putStrLn $ "Haskell: Rust says in withPtr x=" ++ show x
--      xContent <- peek x
--      putStrLn $ "Haskell: Rust says in withPtr x=" ++ show xContent
  putStrLn $ "test vector: " ++ show x
--  putStrLn $ "test vector: " ++ show rx
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ (show v)

-- Utils

vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
vectorToC vec len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec

