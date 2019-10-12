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
    let x = 2
    [rustIO|
      k = $(x : i32) + 1
      print *, "adalah dia", k
    |]
    putStrLn $ "Haskell: Rust says in withPtr " ++ show x
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show (rr :: Int32)

