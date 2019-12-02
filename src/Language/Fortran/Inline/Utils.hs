{-|
Module      : Language.Rust.Inline
Description : Quasiquotes for writing Rust code inline in Haskell
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Fortran.Inline.Utils
  where

import qualified Language.C.Inline as C
import Data.ByteString.Internal (ByteString(..))

C.context (C.baseCtx <> C.bsCtx)

C.include "f90split.c"

splitF90 :: ByteString -> IO ()
splitF90 filename = do
  [C.block| void {
      main_f90split($bs-cstr:filename);
                         } |]
  putStrLn "====!splitF90"
