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
  r <- return ()
  rr <- withPtr_ $ \x -> do
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
      real :: v(2,3)
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
          v(i,j) = 10*i + j
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

  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show rr
