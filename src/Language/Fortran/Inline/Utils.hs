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
import Language.Fortran.Inline.TH.Strings
C.context (C.baseCtx <> C.bsCtx)

C.include "<ctype.h>"
C.include "<stdbool.h>"
C.include "<stdio.h>"
C.include "<stdlib.h>"
C.include "<string.h>"
C.include "<unistd.h>"

C.verbatim sourceC

splitF90 :: ByteString -> IO ()
splitF90 filename = do
  [C.block| void {
      main_f90split($bs-cstr:filename);
                         } |]
  putStrLn "====!splitF90"

