{-# LANGUAGE CPP #-}

-- | @interruptible@ variants of the "Language.Fortran.Inline" quasi-quoters, to call
-- interruptible C code. See <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi.html#ffi-interruptible>
-- for more information.
--
-- This module is intended to be imported qualified:
--
-- @
-- import qualified "Language.Fortran.Inline.Interruptible" as CI
-- @

module Language.Fortran.Inline.Interruptible
  ( exp
  , pure
  , block
  ) where

#if __GLASGOW_HASKELL__ < 710
import           Prelude hiding (exp)
#else
import           Prelude hiding (exp, pure)
#endif

import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH

import           Language.Fortran.Inline.Context
import           Language.Fortran.Inline.Internal

-- | C expressions.
exp :: TH.QuasiQuoter
exp = genericQuote IO $ inlineExp TH.Interruptible

-- | Variant of 'exp', for use with expressions known to have no side effects.
--
-- BEWARE: use this function with caution, only when you know what you are
-- doing. If an expression does in fact have side-effects, then indiscriminate
-- use of 'pure' may endanger referential transparency, and in principle even
-- type safety.
pure :: TH.QuasiQuoter
pure = genericQuote Pure $ inlineExp TH.Interruptible

-- | C code blocks (i.e. statements).
block :: TH.QuasiQuoter
block = genericQuote IO $ inlineItems TH.Unsafe False Nothing
