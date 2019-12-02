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
import Foreign.C.String

C.context (C.baseCtx <> C.funCtx)

C.include "f90split.c"

splitF90 :: IO ()
splitF90 = do
  let reverseIO :: CString -> IO CString
      reverseIO cs = peekCString cs >>= return. reverse >>= newCString
  {-
  [C.block| void {
      pageturnerui($fun:(char* (*reverseIO)(char *)));
                         } |]
                         -}
  putStrLn "====!splitF90"
