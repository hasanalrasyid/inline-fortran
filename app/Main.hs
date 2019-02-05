{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Fortran.Inline
import Data.Int

extendContext basic

setCrateRoot []

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  x <- readLn
  z <- readLn
  y <- [rustIO| i32 {
      double precision a,b,c,eps
          a = 4.0d0/3.0d0
   10 b = a - 1.0d0
      c = b + b + b
      eps = dabs(c-1.0d0)
      if (eps .eq. 0.0d0) go to 10
      ret = eps*dabs(x)
      return
                    } |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y

