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
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign

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


vectorFromC :: Storable a => Int -> Ptr a -> IO (V.Vector a)
vectorFromC len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.freeze $ VM.unsafeFromForeignPtr0 ptr' len

vectorToC :: Storable a => V.Vector a -> Int -> Ptr a -> IO ()
vectorToC vec len ptr = do
  ptr' <- newForeignPtr_ ptr
  V.copy (VM.unsafeFromForeignPtr0 ptr' len) vec
