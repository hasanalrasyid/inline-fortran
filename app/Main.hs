{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Rust.Inline
import Data.Int

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  x <- readLn
  y <- [rustIO|
    k = $(x: i32) + 1
    |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y

