{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int
import Language.C.Inline
import Foreign

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  let i = 55
  r <- return ()
  rr <- withPtr_ $ \x -> do
    poke x 75
    [rustIO|
      print *, "adalah dianya yang sepertinya"
      print *, "adalah dianya yang sepertinya ",x
      k = $(x : i32) + 5
      x = x + k
    |]
      -- k = 5 + $(x : i32) # anehnya, ini error
    putStrLn $ "Haskell: Rust says in withPtr x=" ++ show x
    xContent <- peek x
    putStrLn $ "Haskell: Rust says in withPtr x=" ++ show xContent
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show rr
