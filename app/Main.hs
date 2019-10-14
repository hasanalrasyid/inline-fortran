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
    [rustIO|
      print *, "adalah dianya yang sepertinya"
      print *, "adalah dianya yang sepertinya ",x
      k = 1;
      $(x:out:real) = k + 5
      print *, "adalah dianya yang sepertinya ",k
    |]
      -- k = 5 + $(x : i32) # anehnya, ini error
    putStrLn $ "Haskell: Rust says in withPtr x=" ++ show x
    xContent <- peek x
    putStrLn $ "Haskell: Rust says in withPtr x=" ++ show xContent
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show rr
