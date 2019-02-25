{-|
Module      : Language.Fortran.Inline.Pretty
Description : Utility functions for pretty-printing
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}

module Language.Inline.Pretty (
  renderType,
  renderFType,
  renderItem,
  renderTokens,
) where

import qualified Language.Fortran.ParserMonad as FPM
import qualified Language.Fortran.AST as F
import qualified Language.Fortran.PrettyPrint as FPP

import Language.Rust.Pretty                    ( Pretty(..) )
import Language.Rust.Data.Position             ( Spanned(..) )
import Language.Rust.Syntax                    ( Ty, Token(..), TokenTree(..), TokenStream(..), Item )

import Data.Text.Prettyprint.Doc               ( layoutPretty, defaultLayoutOptions )
import Data.Text.Prettyprint.Doc.Render.String ( renderString )

-- | Render a something that is 'Pretty' into a 'String'
render :: Pretty a => a -> String
render = renderString . layoutPretty defaultLayoutOptions . prettyUnresolved

-- | Render a Rust type into a 'String'.
renderType :: Ty a -> String
renderType = render
renderFType :: F.BaseType -> String
renderFType t = FPP.pprintAndRender FPM.Fortran95 t Nothing

-- | Render a Rust item into a 'String'.
renderItem :: Item a -> String
renderItem = render

-- | Render a sequence of Rust 'Token's into a 'String'.
renderTokens :: [Spanned Token] -> String
renderTokens toks = render (Stream [ Tree (Token t s) |  Spanned s t <- toks ])
