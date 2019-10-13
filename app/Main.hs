{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int
import Language.C.Inline

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  let i = 55
  r <- return ()
  rr <- withPtr_ $ \x -> do
    [rustIO|
      print *, "adalah dianya yang sepertinya"
      print *, "adalah dianya yang sepertinya ",x
      k = $(x : i32) + 5
      x = 5
    |]
    putStrLn $ "Haskell: Rust says in withPtr x=" ++ show x
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show rr
