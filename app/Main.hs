{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  let x = 55
  y <- [rustIO|
    k = $(x: i32) + 1
    |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y

