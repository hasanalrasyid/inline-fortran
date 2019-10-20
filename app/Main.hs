{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int
import Language.C.Inline
import Foreign

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
      IMPLICIT iNTEGER (K-R)
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
    |]
      -- k = 5 + $(x : i32) # anehnya, ini error
    putStrLn $ "Haskell: Rust says in withPtr x=" ++ show x
    xContent <- peek x
    putStrLn $ "Haskell: Rust says in withPtr x=" ++ show xContent

  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show rr
