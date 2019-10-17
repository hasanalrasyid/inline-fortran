{-|
Module      : Language.Rust.Quote
Description : Quasiquotes for Rust AST
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Quasiquoters for converting Rust code into the equivalent Haskell patterns and expressions.
These are just convenience wrappers over 'dataToExpQ' and 'dataToPatQ'. These quasiquoters
only work as expressions and patterns, not as declarations or types. The pattern quasiquoters
recursively ignore any 'Span' or 'Position' fields (replacing them with wildcard patterns).

Using quasiquotes instead of manually writing out the AST means that even if the AST evolves
(perhaps by adding extra fields to certain nodes), your code is likely to continue to work.

The examples below assume the following GHCi flag and import:

>>> :set -XQuasiQuotes
>>> import Control.Monad ( void )
-}


module Language.Fortran.Quote (
--  lit,
  attr,
  ty,
--  pat, stmt, expr, item, sourceFile, implItem, traitItem, tokenTree, block
) where

{-
In the future, we may try to do something similar to Rust macros to extract or inject ASTs out or
into the quasiquotes.

Eventually, one should be able to just import this module for code generation. The following
interaction is what should eventually work.

>>> import qualified Language.Rust.Quote as Q
>>> :set -XQuasiQuotes +t
>>> let one = [Q.expr| 1i32 |]
one :: Expr Span
>>> [Q.expr| |x: i32| -> $retTy:ty $body:block |] = [Q.expr| |x: i32| -> i32 { ($one) + x } |]
retTy :: Ty Span
body :: Block Span
>>> import Language.Rust.Pretty
>>> pretty retTy
i32
>>> pretty body
{ (1i32) + x }

For now, however, you cannot use @$x@ or @$x:ty@ meta variables.
-}

import Language.Fortran.Parser.ParseMonad
import Language.Fortran.Parser.Internal
import Language.Rust.Data.InputStream   ( inputStreamFromString )
import Language.Rust.Data.Position      ( Position(..), Span )

import Language.Haskell.TH
import Language.Haskell.TH.Quote        ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

import Control.Applicative              ( (<|>) )
import Control.Monad                    ( (>=>) )
import Data.Functor                     ( ($>) )
import Data.Typeable                    ( cast, Typeable )
import Data.Data                        ( Data )

-- | Given a parser, convert it into a quasiquoter. The quasiquoter produced does not support
-- declarations and types. For patterns, it replaces any 'Span' and 'Position' field with a
-- wild pattern.
quoter :: Data a => P a -> QuasiQuoter
quoter p = QuasiQuoter
             { quoteExp = parse >=> dataToExpQ (const Nothing)
             , quotePat = parse >=> dataToPatQ wildSpanPos
             , quoteDec = error "this quasiquoter does not support declarations"
             , quoteType = error "this quasiquoter does not support types"
             }
  where
  -- | Given a parser and an input string, turn it into the corresponding Haskell expression/pattern.
  parse inp = do
    Loc{ loc_start = (r,c) } <- location

    -- Run the parser
    case execParser p (inputStreamFromString inp) (Position 0 r c) of
      Left (ParseFail _ msg) -> fail msg
      Right x -> pure x

  -- | Replace 'Span' and 'Position' with wild patterns
  wildSpanPos :: Typeable b => b -> Maybe (Q Pat)
  wildSpanPos x = ((cast x :: Maybe Span) $> wildP) <|> ((cast x :: Maybe Position) $> wildP)


-- | Quasiquoter for literals (see 'Language.Rust.Syntax.Lit').
--
-- >>> void [lit| 1.4e29f64 |]
-- Float 1.4e29 F64 ()
--
--h--lit :: QuasiQuoter
--h--lit = quoter parseLit
--h--
--h---- | Quasiquoter for attributes (see 'Language.Rust.Syntax.Attribute')
--h----
--h---- >>> void [attr| #[no_mangle] |]
--h---- Attribute Outer (Path False [PathSegment "no_mangle" Nothing ()] ()) (Stream []) ()
--
attr :: QuasiQuoter
attr = quoter parseAttr

-- | Quasiquoter for types (see 'Language.Rust.Syntax.Ty')
--
-- >>> void [ty| &(_,_) |]
-- Rptr Nothing Immutable (TupTy [Infer (),Infer ()] ()) ()
--
ty :: QuasiQuoter
ty = quoter parseTy

-- | Quasiquoter for patterns (see 'Language.Rust.Syntax.Pat')
--
-- >>> void [pat| x @ 1...5 |]
-- IdentP (ByValue Immutable) "x" (Just (RangeP (Lit [] (Int Dec 1 Unsuffixed ()) ())
--                                              (Lit [] (Int Dec 5 Unsuffixed ()) ()) ())) ()
--
--h--pat :: QuasiQuoter
--h--pat = quoter parsePat
--h--
--h---- | Quasiquoter for statements (see 'Language.Rust.Syntax.Stmt')
--h----
--h---- >>> void [stmt| let x = 4i32; |]
--h---- Local (IdentP (ByValue Immutable) "x" Nothing ()) Nothing (Just (Lit [] (Int Dec 4 I32 ()) ())) [] ()
--h----
--h--stmt :: QuasiQuoter
--h--stmt = quoter parseStmt
--h--
--h---- | Quasiquoter for expressions (see 'Language.Rust.Syntax.Expr')
--h----
--h---- >>> void [expr| (x,) |]
--h---- TupExpr [] [PathExpr [] Nothing (Path False [PathSegment "x" Nothing ()] ()) ()] ()
--h----
--h--expr :: QuasiQuoter
--h--expr = quoter parseExpr
--h--
-- | Quasiquoter for items (see 'Language.Rust.Syntax.Item')
--
-- >>> void [item| type Unit = (); |]
-- TyAlias [] InheritedV "Unit" (TupTy [] ()) (Generics [] [] (WhereClause [] ()) ()) ()
--
--h--item :: QuasiQuoter
--h--item = quoter parseItem
--h--
--h---- | Quasiquoter for a whole source file (see 'Language.Rust.Syntax.SourceFile')
--h----
--h---- >>> void [sourceFile| fn main() { } |]
--h---- SourceFile Nothing [] [Fn [] InheritedV "main"
--h----                           (FnDecl [] Nothing False ())
--h----                           Normal NotConst Rust
--h----                           (Generics [] [] (WhereClause [] ()) ())
--h----                           (Block [] Normal ()) ()]
--h----
--h--sourceFile :: QuasiQuoter
--h--sourceFile = quoter parseSourceFile
--h--
--h---- | Quasiquoter for blocks (see 'Language.Rust.Syntax.Block')
--h----
--h---- >>> void [block| unsafe { 1i32 } |]
--h---- Block [NoSemi (Lit [] (Int Dec 1 I32 ()) ()) ()] Unsafe ()
--h----
--h--block :: QuasiQuoter
--h--block = quoter parseBlock
--h--
--h---- | Quasiquoter for impl items (see 'Language.Rust.Syntax.ImplItem')
--h----
--h---- >>> void [implItem| type Item = (); |]
--h---- TypeI [] InheritedV Final "Item" (TupTy [] ()) ()
--h----
--h--implItem :: QuasiQuoter
--h--implItem = quoter parseImplItem
--h--
--h---- | Quasiquoter for trait items (see 'Language.Rust.Syntax.TraitItem')
--h----
--h---- >>> void [traitItem| type Item; |]
--h---- TypeT [] "Item" [] Nothing ()
--h----
--h--traitItem :: QuasiQuoter
--h--traitItem = quoter parseTraitItem
--h--
--h---- | Quasiquoter for token trees (see 'Language.Rust.Syntax.TokenTree')
--h----
--h---- >>> [tokenTree| fn |]
--h---- Token (Span (Position 1 2 14) (Position 3 2 16)) fn
--h----
--h--tokenTree :: QuasiQuoter
--h--tokenTree = quoter parseTt

