{-|
Module      : Language.Rust.Syntax
Description : Syntax data defintions
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

This module defines Haskell data types corresponding to the abstract syntax tree(s) of the Rust
language, based on the definitions @rustc@ uses (defined in @libsyntax@) whenever possible.
Unfortunately, since the internals of @rustc@ are not exposed, there are no official
docs. <Here https://manishearth.github.io/rust-internals-docs/syntax/ast/index.html> are the
unofficial docs.
-}

module Language.Fortran.Syntax (
  -- * Abstract syntax trees
  module Language.Fortran.Syntax.AST,
  -- * Tokens
  module Language.Fortran.Syntax.Token,
) where

import Language.Fortran.Syntax.AST
import Language.Fortran.Syntax.Token

-- Using import/export shortcut screws up Haddock
{-# ANN module "HLint: ignore Use import/export shortcut" #-}

