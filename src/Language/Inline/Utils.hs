{-|
Module      : Language.Fortran.Inline.Internal
Description : Manages the module-level state
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Inline.Utils (
  debugIt
) where


import Language.Haskell.TH
import Language.Haskell.TH.Syntax

--import Control.Monad               ( when )
import Data.Typeable               ( Typeable )
--import Data.Monoid                 ( Endo(..) )
import Data.Maybe                  ( fromMaybe )
import Data.List                   ( unfoldr )
import Data.Char                   ( isAlpha, isAlphaNum )

import System.FilePath             ( (</>), (<.>), takeExtension )
import System.Directory            ( copyFile, createDirectoryIfMissing )
--import System.Process              ( spawnProcess, readProcess, waitForProcess )
import System.Process              ( readProcess)
--import System.Exit                 ( ExitCode(..) )
import System.Environment          ( setEnv )

import Text.JSON


debugIt :: forall a. (Show a) =>  String ->  [a] -> Q ()
debugIt a b = runIO $
    putStrLn $ unlines $ a : map (('+':) . show) b


