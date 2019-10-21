{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int
import Language.C.Inline
import Foreign
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

extendContext basic

setCrateRoot []

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  let i = 55
  let vInit = V.fromList [1,2,3,4,5,6] :: V.Vector Float
  vi <- V.thaw vInit
  rr <- VM.unsafeWith vi $ \v -> do
        rx <- withPtr_ $ \x -> do

                poke x i
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
      k = 1
      # 373 test
      $(x:out:real) = k + 5
      write(*,10001) k,
     & x
10001   FORMAT('IDLING TIME : ',I10,' sec (',F6.2,' %)')
      print *, "adalah ",k

      do 300 i = 1,2
      do 301 j = 1,3
          $vec(v:inout:real:(2,3))(i,j) = 10*i + j
  301   continue
  300 continue
      print *, "adalah dianya yang "
      do 400 i = 1,2
      do 401 j = 1,3
          print*,"test v:", v(i,j)
  401   continue
  400 continue

          |]
      -- k = 5 + $(x : i32) # anehnya, ini error
                putStrLn $ "Haskell: Rust says in withPtr x=" ++ show x
                xContent <- peek x
                putStrLn $ "Haskell: Rust says in withPtr x=" ++ show xContent
        putStrLn $ "test vector: " ++ show rx
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show rr

-- Utils

vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
vectorToC vec len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec

