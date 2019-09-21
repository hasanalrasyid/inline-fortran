{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Language.Inline
import Data.Int

extendContext basic
extendContext ghcUnboxed
--extendContext basicF

setCrateRoot []

main = do
  putStrLn "Haskell: Hello. Enter a number:"
  x <- readLn
  y <- [fortIO| i32 {
    println!("Rust: Your number is {}", $(x: i32));
    $(x: i32) + 1
  } |]
  putStrLn $ "Haskell: Rust says number plus 1 is " ++ show y

