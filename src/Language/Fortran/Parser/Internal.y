{
{-|
Module      : Language.Rust.Parser.Internal
Description : Rust parser
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

The parsers in this file are all re-exported to 'Language.Rust.Parser' via the 'Parse' class. The
parsers are based off of:

  * primarily the reference @rustc@ [implementation][0]
  * some documentation on [rust-lang][2]
  * drawing a couple ideas from a slightly outdated [ANTLR grammar][1]

To get information about transition states and such, run

>  happy --info=happyinfo.txt -o /dev/null src/Language/Rust/Parser/Internal.y

  [0]: https://github.com/rust-lang/rust/blob/master/src/libsyntax/parse/parser.rs
  [1]: https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y
  [2]: https://doc.rust-lang.org/grammar.html
-}
{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Fortran.Parser.Internal (
  -- * Parsers
  parseAttr,
--  parseBlock,
--  parseExpr,
--  parseGenerics,
--  parseImplItem,
--  parseItem,
--  parseLifetimeDef,
--  parseLit,
--  parsePat,
--  parseSourceFile,
--  parseStmt,
--  parseTokenStream,
--  parseTraitItem,
--  parseTt,
--  parseTyParam,
--  parseWhereClause,
  parseTy
) where

import Language.Fortran.Syntax

import Language.Rust.Data.Ident        ( Ident(..), mkIdent )
import Language.Rust.Data.Position

import Language.Fortran.Parser.Lexer      ( lexNonSpace, lexShebangLine )
import Language.Fortran.Parser.ParseMonad ( pushToken, getPosition, P, parseError )
import Language.Fortran.Parser.Literals   ( translateLit )
import Language.Fortran.Parser.Reversed

import Data.Foldable                   ( toList )
import Data.List                       ( (\\), isSubsequenceOf, sort )
import Data.Semigroup                  ( (<>) )

import Text.Read                       ( readMaybe )

import Data.List.NonEmpty              ( NonEmpty(..), (<|) )
import qualified Data.List.NonEmpty as N
}

-- in order to document the parsers, we have to alias them
%name parseTy ty
--h--%name parseLit lit
%name parseAttr export_attribute
--h--%name parsePat pat
--h--%name parseStmt stmt
--h--%name parseExpr expr
--h--%name parseItem mod_item
--h--%name parseSourceFileContents source_file
--h--%name parseBlock export_block
--h--%name parseImplItem  impl_item
--h--%name parseTraitItem trait_item
--h--%name parseTt token_tree
--h--%name parseTokenStream token_stream
--h--%name parseTyParam ty_param
--h--%name parseLifetimeDef lifetime_def
--h--%name parseWhereClause where_clause
--h--%name parseGenerics generics

%tokentype { Spanned Token }
%lexer { lexNonSpace >>= } { Spanned Eof _ }
%monad { P } { >>= } { return }

%errorhandlertype explist
%error { expParseError }

%expect 0

%token

  -- Expression-operator symbols.
  '='            { Spanned Equal _ }
  '<'            { Spanned Less _ }
  '>'            { Spanned Greater _ }
  '!'            { Spanned Exclamation _ }
  '~'            { Spanned Tilde _ }

  '+'            { Spanned Plus _ }
  '-'            { Spanned Minus _ }
  '*'            { Spanned Star _ }
  '/'            { Spanned Slash _ }
  '%'            { Spanned Percent _ }
  '^'            { Spanned Caret _ }
  '&'            { Spanned Ampersand _ }
  '|'            { Spanned Pipe _ }

  -- Structural symbols.
  '@'            { Spanned At _ }
  '...'          { Spanned DotDotDot _ }
  '..='          { Spanned DotDotEqual _ }
  '..'           { Spanned DotDot _ }
  '.'            { Spanned Dot _ }
  ','            { Spanned Comma _ }
  ';'            { Spanned Semicolon _ }
  '::'           { Spanned ModSep _ }
  ':'            { Spanned Colon _ }
  '->'           { Spanned RArrow _ }
  '<-'           { Spanned LArrow _ }
  '=>'           { Spanned FatArrow _ }
  '#'            { Spanned Pound _ }
  '$'            { Spanned Dollar _ }
  '?'            { Spanned Question _ }
  '#!'           { Spanned Shebang _ }

  '||'           { Spanned PipePipe _ }
  '&&'           { Spanned AmpersandAmpersand _ }
  '>='           { Spanned GreaterEqual _ }
  '>>='          { Spanned GreaterGreaterEqual _ }
  '<<'           { Spanned LessLess _ }
  '>>'           { Spanned GreaterGreater _ }

  '=='           { Spanned EqualEqual _ }
  '!='           { Spanned NotEqual _ }
  '<='           { Spanned LessEqual _ }
  '<<='          { Spanned LessLessEqual _ }
  '-='           { Spanned MinusEqual _ }
  '&='           { Spanned AmpersandEqual _ }
  '|='           { Spanned PipeEqual _ }
  '+='           { Spanned PlusEqual _ }
  '*='           { Spanned StarEqual _ }
  '/='           { Spanned SlashEqual _ }
  '^='           { Spanned CaretEqual _ }
  '%='           { Spanned PercentEqual _ }

  '('            { Spanned (OpenDelim Paren) _ }
  '['            { Spanned (OpenDelim Bracket) _ }
  '{'            { Spanned (OpenDelim Brace) _ }
  ')'            { Spanned (CloseDelim Paren) _ }
  ']'            { Spanned (CloseDelim Bracket) _ }
  '}'            { Spanned (CloseDelim Brace) _ }

  -- Literals.
  byte           { Spanned (LiteralTok ByteTok{} _) _ }
  char           { Spanned (LiteralTok CharTok{} _) _ }
  int            { Spanned (LiteralTok IntegerTok{} _) _ }
  float          { Spanned (LiteralTok FloatTok{} _) _ }
  str            { Spanned (LiteralTok StrTok{} _) _ }
  byteStr        { Spanned (LiteralTok ByteStrTok{} _) _ }
  rawStr         { Spanned (LiteralTok StrRawTok{} _) _ }
  rawByteStr     { Spanned (LiteralTok ByteStrRawTok{} _) _ }

  -- Strict keywords used in the language
  as             { Spanned (IdentTok "as") _ }
  box            { Spanned (IdentTok "box") _ }
  break          { Spanned (IdentTok "break") _ }
  const          { Spanned (IdentTok "const") _ }
  continue       { Spanned (IdentTok "continue") _ }
  crate          { Spanned (IdentTok "crate") _ }
  else           { Spanned (IdentTok "else") _ }
  enum           { Spanned (IdentTok "enum") _ }
  extern         { Spanned (IdentTok "extern") _ }
  false          { Spanned (IdentTok "false") _ }
  fn             { Spanned (IdentTok "fn") _ }
  for            { Spanned (IdentTok "for") _ }
  if             { Spanned (IdentTok "if") _ }
  impl           { Spanned (IdentTok "impl") _ }
  in             { Spanned (IdentTok "in") _ }
  out            { Spanned (IdentTok "out") _ }
  inout          { Spanned (IdentTok "inout") _ }
  let            { Spanned (IdentTok "let") _ }
  loop           { Spanned (IdentTok "loop") _ }
  match          { Spanned (IdentTok "match") _ }
  mod            { Spanned (IdentTok "mod") _ }
  move           { Spanned (IdentTok "move") _ }
  mut            { Spanned (IdentTok "mut") _ }
  pub            { Spanned (IdentTok "pub") _ }
  ref            { Spanned (IdentTok "ref") _ }
  return         { Spanned (IdentTok "return") _ }
  Self           { Spanned (IdentTok "Self") _ }
  self           { Spanned (IdentTok "self") _ }
  static         { Spanned (IdentTok "static") _ }
  struct         { Spanned (IdentTok "struct") _ }
  super          { Spanned (IdentTok "super") _ }
  trait          { Spanned (IdentTok "trait") _ }
  true           { Spanned (IdentTok "true") _ }
  type           { Spanned (IdentTok "type") _ }
  unsafe         { Spanned (IdentTok "unsafe") _ }
  use            { Spanned (IdentTok "use") _ }
  where          { Spanned (IdentTok "where") _ }
  while          { Spanned (IdentTok "while") _ }
  do             { Spanned (IdentTok "do") _ }

  -- Keywords reserved for future use
  abstract       { Spanned (IdentTok "abstract") _ }
  alignof        { Spanned (IdentTok "alignof") _ }
  become         { Spanned (IdentTok "become") _ }
  final          { Spanned (IdentTok "final") _ }
  macro          { Spanned (IdentTok "macro") _ }
  offsetof       { Spanned (IdentTok "offsetof") _ }
  override       { Spanned (IdentTok "override") _ }
  priv           { Spanned (IdentTok "priv") _ }
  proc           { Spanned (IdentTok "proc") _ }
  pure           { Spanned (IdentTok "pure") _ }
  sizeof         { Spanned (IdentTok "sizeof") _ }
  typeof         { Spanned (IdentTok "typeof") _ }
  unsized        { Spanned (IdentTok "unsized") _ }
  virtual        { Spanned (IdentTok "virtual") _ }

  -- Weak keywords, have special meaning only in specific contexts.
  default        { Spanned (IdentTok "default") _ }
  union          { Spanned (IdentTok "union") _ }
  catch          { Spanned (IdentTok "catch") _ }
  auto           { Spanned (IdentTok "auto") _ }
  yield          { Spanned (IdentTok "yield") _ }
  dyn            { Spanned (IdentTok "dyn") _ }

  -- Comments
  outerDoc       { Spanned (Doc _ Outer _) _ }
  innerDoc       { Spanned (Doc _ Inner _) _ }

  -- Identifiers.
  '_'            { Spanned (IdentTok "_") _ }
  IDENT          { Spanned IdentTok{} _ }

  -- Lifetimes.
  LIFETIME       { Spanned (LifetimeTok _) _ }

  -- Interpolated
  ntItem         { Spanned (Interpolated (NtItem $$)) _ }
  ntBlock        { Spanned (Interpolated (NtBlock $$)) _ }
  ntStmt         { Spanned (Interpolated (NtStmt $$)) _ }
  ntPat          { Spanned (Interpolated (NtPat $$)) _ }
  ntExpr         { Spanned (Interpolated (NtExpr $$)) _ }
  ntTy           { Spanned (Interpolated (NtTy $$)) _ }
  ntIdent        { Spanned (Interpolated (NtIdent _)) _ }
  ntPath         { Spanned (Interpolated (NtPath $$)) _ }
  ntTT           { Spanned (Interpolated (NtTT $$)) _ }
  ntArm          { Spanned (Interpolated (NtArm $$)) _ }
  ntImplItem     { Spanned (Interpolated (NtImplItem $$)) _ }
  ntTraitItem    { Spanned (Interpolated (NtTraitItem $$)) _ }
  ntGenerics     { Spanned (Interpolated (NtGenerics $$)) _ }
  ntWhereClause  { Spanned (Interpolated (NtWhereClause $$)) _ }
  ntArg          { Spanned (Interpolated (NtArg $$)) _ }
  ntLit          { Spanned (Interpolated (NtLit $$)) _ }

-- 'SEG' needs to be lower than '::' for path segments
%nonassoc SEG

-- 'mut' needs to be lower precedence than 'IDENT' so that in 'pat', something like "&mut x"
-- associates the "mut" to a refence pattern and not to the identifier pattern "x".
--
-- 'DEF' is for the empty case of 'def', which needs to _not_ be taken when there is a 'default'
-- token available.
--
-- 'EQ' is for differentiating the 'where ty' from 'where ty = ty' case in where clause
-- predicates, since the former needs to _not_ be taken when there is a '=' token available.
--
-- '::' is so that the remainder of mod paths in attributes are not gobbled as just raw tokens
%nonassoc mut DEF EQ '::'

-- These are all identifiers of sorts ('union' and 'default' are "weak" keywords)
%nonassoc IDENT ntIdent default union catch self Self super auto dyn crate

-- These are all very low precedence unary operators
%nonassoc box return yield break continue for IMPLTRAIT LAMBDA

-- 'static' needs to have higher precedenc than 'LAMBDA' so that statements starting in static get
-- considered as static items, and not a static lambda
%nonassoc static

-- These are the usual arithmetic precedences. 'UNARY' is introduced here for '*', '!', '-', '&'
%right '=' '>>=' '<<=' '-=' '+=' '*=' '/=' '^=' '|=' '&=' '%='
%right '<-'
%nonassoc SINGLERNG
%nonassoc INFIXRNG
%nonassoc POSTFIXRNG
%nonassoc PREFIXRNG
%nonassoc '..' '...' '..='
%left '||'
%left '&&'
%left '==' '!=' '<' '>' '<=' '>='
%left '|'
%left '^'
%left '&'
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%nonassoc ':' as
%nonassoc UNARY

-- These are all generated precedence tokens.
--
--  * 'FIELD' for field access expressions (which bind less tightly than '.' for method calls)
--  * 'VIS' for adjusting the precedence of 'pub' compared to other visbility modifiers (see 'vis')
--  * 'PATH' boosts the precedences of paths in types and expressions
--  * 'WHERE' is for non-empty where clauses
--
%nonassoc FIELD VIS PATH WHERE NOSEMI

-- These are postfix operators.
%nonassoc '?' '.'

-- Delimiters have the highest precedence. 'ntBlock' counts as a delimiter since it always starts
-- and ends with '{' and '}'
%nonassoc '{' ntBlock '[' '(' '!' ';'

%%

-- Unwraps the IdentTok into just an Ident
-- For questionable reasons of backwards compatibility, 'union', 'default', and 'catch' can be used
-- as identifiers, even if they are also keywords. They are "contextual" keywords.
--
-- Union's RFC: https://github.com/rust-lang/rfcs/blob/master/text/1444-union.md
ident :: { Spanned Ident }
  : ntIdent                       { fmap (\(Interpolated (NtIdent i)) -> i) $1 }
  | union                         { toIdent $1 }
  | default                       { toIdent $1 }
  | catch                         { toIdent $1 }
  | auto                          { toIdent $1 }
  | dyn                           { toIdent $1 }
  | IDENT                         { toIdent $1 }

--h---- This should precede any '>' token which could be absorbed in a '>>', '>=', or '>>=' token. Its
--h---- purpose is to check if the lookahead token starts with '>' but contains more that. If that is
--h---- the case, it pushes two tokens, the first of which is '>'. We exploit the %% feature of threaded
--h---- lexers to discard what would have been the troublesome '>>', '>=', or '>>=' token.
--h--gt :: { () }
--h--  : {- empty -}   {%% \(Spanned tok s) ->
--h--      let s' = nudge 1 0 s; s'' = nudge 0 (-1) s in
--h--      case tok of
--h--        GreaterGreater      -> pushToken (Spanned Greater s')      *> pushToken (Spanned Greater s'')
--h--        GreaterEqual        -> pushToken (Spanned Equal s')        *> pushToken (Spanned Greater s'')
--h--        GreaterGreaterEqual -> pushToken (Spanned GreaterEqual s') *> pushToken (Spanned Greater s'')
--h--        _                   -> pushToken (Spanned tok s)
--h--    }
--h--
--h---- This should precede any '|' token which could be absorbed in a '||' token. This works in the same
--h---- way as 'gt'.
--h--pipe :: { () }
--h--  : {- empty -}   {%% \(Spanned tok s) ->
--h--      let s' = nudge 1 0 s; s'' = nudge 0 (-1) s in
--h--      case tok of
--h--        PipePipe -> pushToken (Spanned Pipe s') *> pushToken (Spanned Pipe s'')
--h--        _        -> pushToken (Spanned tok s)
--h--    }

-------------
-- Utility --
-------------

-- | One or more occurences of 'p'
some(p) :: { Reversed NonEmpty _ }
  : some(p) p             { let Reversed xs = $1 in Reversed ($2 <| xs) }
  | p                     { [$1] }

--h---- | Zero or more occurences of 'p'
--h--many(p) :: { [ _ ] }
--h--  : some(p)               { toList $1 }
--h--  | {- empty -}           { [] }
--h--
-- | One or more occurences of 'p', seperated by 'sep'
sep_by1(p,sep) :: { Reversed NonEmpty _ }
  : sep_by1(p,sep) sep p  { let Reversed xs = $1 in Reversed ($3 <| xs) }
  | p                     { [$1] }

--h---- | Zero or more occurrences of 'p', separated by 'sep'
--h--sep_by(p,sep) :: { [ _ ] }
--h--  : sep_by1(p,sep)        { toList $1 }
--h--  | {- empty -}           { [] }

-- | One or more occurrences of 'p', seperated by 'sep', optionally ending in 'sep'
sep_by1T(p,sep) :: { Reversed NonEmpty _ }
  : sep_by1(p,sep) sep    { $1 }
  | sep_by1(p,sep)        { $1 }

--h---- | Zero or more occurences of 'p', seperated by 'sep', optionally ending in 'sep' (only if there
--h---- is at least one 'p')
--h--sep_byT(p,sep) :: { [ _ ] }
--h--  : sep_by1T(p,sep)       { toList $1 }
--h--  | {- empty -}           { [] }
--h--
--h--
--h----------------------------
--h---- Whole file
--h----------------------------
--h--
--h---- shebang is dealt with at the top level, outside Happy/Alex
--h--source_file :: { ([Attribute Span],[Item Span]) }
--h--  : inner_attrs many(mod_item)   { (toList $1, $2) }
--h--  |             many(mod_item)   { ([],        $1) }
--h--
--h--
--------------------------
-- Attributes
--------------------------

outer_attribute :: { Attribute Span }
  : '#' '[' mod_path token_stream ']'         { Attribute Outer $3 $4 ($1 # $>) }
  | outerDoc                                  { let Spanned (Doc str _ l) x = $1 in SugaredDoc Outer l str x }

inner_attribute :: { Attribute Span }
  : '#' '!' '[' mod_path token_stream ']'     { Attribute Inner $4 $5 ($1 # $>) }
  | '#!'    '[' mod_path token_stream ']'     { Attribute Inner $3 $4 ($1 # $>) }
  | innerDoc                                  { let Spanned (Doc str _ l) x = $1 in SugaredDoc Inner l str x }

--h---- TODO: for some precedence related reason, using 'some' here doesn't work
--h--inner_attrs :: { Reversed NonEmpty (Attribute Span) }
--h--  : inner_attrs inner_attribute               { let Reversed xs = $1 in Reversed ($2 <| xs) }
--h--  | inner_attribute                           { [$1] }
--h--
--h--
--h----------------
--h---- Literals --
--h----------------
--h--
--h--lit :: { Lit Span }
--h--  : ntLit             { $1 }
--h--  | byte              { lit $1 }
--h--  | char              { lit $1 }
--h--  | int               { lit $1 }
--h--  | float             { lit $1 }
--h--  | true              { lit $1 }
--h--  | false             { lit $1 }
--h--  | string            { $1 }
--h--
--h--string :: { Lit Span }
--h--  : str               { lit $1 }
--h--  | rawStr            { lit $1 }
--h--  | byteStr           { lit $1 }
--h--  | rawByteStr        { lit $1 }
--h--
--h--
--h-------------
--h---- Paths --
--h-------------
--h--
--h---- parse_qualified_path(PathStyle::Type)
--h---- qual_path :: Spanned (NonEmpty (Ident, PathParameters Span)) -> P (Spanned (QSelf Span, Path Span))
--h--qual_path(segs) :: { Spanned (QSelf Span, Path Span) }
--h--  : '<' qual_path_suf(segs)                    { let Spanned x _ = $2 in Spanned x ($1 # $2) }
--h--  | lt_ty_qual_path as ty_path '>' '::' segs   {
--h--      let Path g segsTy x = $3 in
--h--      Spanned (QSelf (unspan $1) (length segsTy), Path g (segsTy <> toList $6) x) ($1 # $>)
--h--    }
--h--
--h---- Basically a qualified path, but ignoring the very first '<' token
--h--qual_path_suf(segs) :: { Spanned (QSelf Span, Path Span) }
--h--  : ty '>' '::' segs                { Spanned (QSelf $1 0, Path False (toList $4) (spanOf $4)) ($1 # $>) }
--h--  | ty as ty_path '>' '::' segs     {
--h--      let Path g segsTy x = $3 in
--h--      Spanned (QSelf $1 (length segsTy), Path g (segsTy <> toList $6) x) ($1 # $>)
--h--    }
--h--
--h---- Usually qual_path_suf is for... type paths! This consumes these but with a starting '<<' token.
--h---- The underlying type has the right 'Span' (it doesn't include the very first '<', while the
--h---- 'Spanned' wrapper does)
--h--lt_ty_qual_path :: { Spanned (Ty Span) }
--h--  : '<<' qual_path_suf(path_segments_without_colons)
--h--    { let (qself,path) = unspan $2 in Spanned (PathTy (Just qself) path (nudge 1 0 ($1 # $2))) ($1 # $2) }
--h--
--h---- parse_generic_args() but with the '<' '>'
--h--generic_values :: { Spanned ([Lifetime Span], [Ty Span], [(Ident, Ty Span)]) }
--h--  : '<' sep_by1(lifetime,',')  ',' sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>'
--h--    { Spanned (toList $2,      toList $4, toList $6) ($1 # $>) }
--h--  | '<' sep_by1(lifetime,',')  ',' sep_by1T(ty,',')                          gt '>'
--h--    { Spanned (toList $2,      toList $4, []       ) ($1 # $>) }
--h--  | '<' sep_by1(lifetime,',')  ','                     sep_by1T(binding,',') gt '>'
--h--    { Spanned (toList $2,      [],        toList $4) ($1 # $>) }
--h--  | '<' sep_by1T(lifetime,',')                                               gt '>'
--h--    { Spanned (toList $2,      [],        []       ) ($1 # $>) }
--h--  | '<'                            sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>'
--h--    { Spanned ([],             toList $2, toList $4) ($1 # $>) }
--h--  | '<'                            sep_by1T(ty,',')                          gt '>'
--h--    { Spanned ([],             toList $2, []       ) ($1 # $>) }
--h--  | '<'                                                sep_by1T(binding,',') gt '>'
--h--    { Spanned ([],             [],        toList $2) ($1 # $>) }
--h--  | '<'                                                                      gt '>'
--h--    { Spanned ([],             [],        []       ) ($1 # $>) }
--h--  | lt_ty_qual_path            ',' sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>'
--h--    { Spanned ([], unspan $1 : toList $3, toList $5) ($1 # $>) }
--h--  | lt_ty_qual_path            ',' sep_by1T(ty,',')                          gt '>'
--h--    { Spanned ([], unspan $1 : toList $3, []       ) ($1 # $>) }
--h--  | lt_ty_qual_path                                ',' sep_by1T(binding,',') gt '>'
--h--    { Spanned ([],            [unspan $1],toList $3) ($1 # $>) }
--h--  | lt_ty_qual_path                                                          gt '>'
--h--    { Spanned ([],            [unspan $1],[]       ) ($1 # $>) }
--h--
--h--binding :: { (Ident, Ty Span) }
--h--  : ident '=' ty                             { (unspan $1, $3) }
--h--
--h--
--h---- Type related:
--h---- parse_path(PathStyle::Type)
--h--ty_path :: { Path Span }
--h--  : ntPath                                   { $1 }
--h--  | path_segments_without_colons             { Path False $1 (spanOf $1) }
--h--  | '::' path_segments_without_colons        { Path True $2 ($1 # $2) }
--h--
--h--ty_qual_path :: { Spanned (QSelf Span, Path Span) }
--h--  : qual_path(path_segments_without_colons)  { $1 }
--h--
--h---- parse_path_segments_without_colons()
--h--path_segments_without_colons :: { [PathSegment Span] }
--h--  : sep_by1(path_segment_without_colons, '::') %prec SEG  { toList $1 }
--h--
--h---- No corresponding function - see path_segments_without_colons
--h--path_segment_without_colons :: { PathSegment Span }
--h--  : self_or_ident path_parameter                     { PathSegment (unspan $1) $2 ($1 # $>) }
--h--
--h--path_parameter  :: { Maybe (PathParameters Span) }
--h--  : generic_values                           { let (lts, tys, bds) = unspan $1
--h--                                               in Just (AngleBracketed lts tys bds (spanOf $1)) }
--h--  | '(' sep_byT(ty,',') ')'                  { Just (Parenthesized $2 Nothing ($1 # $>)) }
--h--  | '(' sep_byT(ty,',') ')' '->' ty_no_plus  { Just (Parenthesized $2 (Just $>) ($1 # $>)) }
--h--  | {- empty -}                  %prec IDENT { Nothing }
--h--
--h--
--h---- Expression related:
--h---- parse_path(PathStyle::Expr)
--h--expr_path :: { Path Span }
--h--  : ntPath                                   { $1 }
--h--  | path_segments_with_colons                { Path False (toList $1) (spanOf $1) }
--h--  | '::' path_segments_with_colons           { Path True (toList $2) ($1 # $2) }
--h--
--h--expr_qual_path :: { Spanned (QSelf Span, Path Span) }
--h--  : qual_path(path_segments_with_colons)     { $1 }
--h--
--h---- parse_path_segments_with_colons()
--h--path_segments_with_colons :: { Reversed NonEmpty (PathSegment Span) }
--h--  : self_or_ident
--h--    { [PathSegment (unspan $1) Nothing (spanOf $1)] }
--h--  | path_segments_with_colons '::' self_or_ident
--h--    { $1 <> [PathSegment (unspan $3) Nothing (spanOf $3)] }
--h--  | path_segments_with_colons '::' generic_values
--h--    {%
--h--      case (unsnoc $1, unspan $3) of
--h--        ((rst, PathSegment i Nothing x), (lts, tys, bds)) ->
--h--          let seg = PathSegment i (Just (AngleBracketed lts tys bds (spanOf $3))) (x # $3)
--h--          in pure $ snoc rst seg
--h--        _ -> fail "invalid path segment in expression path"
--h--    }
--h--
--h---- Mod related:
--h---- parse_path(PathStyle::Mod)
--h----
--h---- TODO: This is O(n^2) in the segment length! I haven't been able to make the grammar work out in
--h----       order to refactor this nicely
mod_path :: { Path Span  }
  : ntPath               { $1 }
  | self_or_ident        { Path False [PathSegment (unspan $1) Nothing (spanOf $1)] (spanOf $1) }
  | '::' self_or_ident   { Path True  [PathSegment (unspan $2) Nothing (spanOf $2)] ($1 # $>) }
  | mod_path '::' self_or_ident  {
      let Path g segs _ = $1 in
      Path g (segs <> [PathSegment (unspan $3) Nothing (spanOf $3) ]) ($1 # $3)
    }

self_or_ident :: { Spanned Ident }
  : ident                   { $1 }
  | crate                   { Spanned "crate" (spanOf $1) }
  | self                    { Spanned "self" (spanOf $1) }
  | Self                    { Spanned "Self" (spanOf $1) }
  | super                   { Spanned "super" (spanOf $1) }

--h--
--h-------------
--h---- Types --
--h-------------
--h--
--h--lifetime :: { Lifetime Span }
--h--  : LIFETIME                         { let Spanned (LifetimeTok (Ident l _ _)) s = $1 in Lifetime l s }
--h--
--h---- parse_trait_ref()
--h--trait_ref :: { TraitRef Span }
--h--  : ty_path                          { TraitRef $1 }
--h--
-- parse_ty()
-- See https://github.com/rust-lang/rfcs/blob/master/text/0438-precedence-of-plus.md
-- All types, including trait types with plus
ty :: { Ty Span }
  : ty_no_plus                                                    { $1 }
--  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+') { TraitObject ($1 <| toNonEmpty $3) ($1 # $3) }

-- parse_ty_no_plus()
ty_no_plus :: { Ty Span }
  : ntTy                             { $1 }
  | no_for_ty                        { $1 }
--  | for_ty_no_plus                   { $1 }

--h---- All types not starting with a '(' or '<'
--h--ty_prim :: { Ty Span }
--h--  : no_for_ty_prim                   { $1 }
--h--  | for_ty_no_plus                   { $1 }
--h--  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+') { TraitObject ($1 <| toNonEmpty $3) ($1 # $3) }
--h--
-- All (non-sum) types not starting with a 'for'
no_for_ty :: { Ty Span }
--  : no_for_ty_prim                   { $1 }
  : ident                            { Infer (spanOf $1) }
  | '(' ')'                          { TupTy [] ($1 # $>) }
  | '(' ty ')'                       { ParenTy $2 ($1 # $3) }
  | '(' ty ',' ')'                   { TupTy [$2] ($1 # $4) }
  | '(' ty ',' sep_by1T(ty,',') ')'  { TupTy ($2 : toList $4) ($1 # $5) }
--  | ty_qual_path                     { PathTy (Just (fst (unspan $1))) (snd (unspan $1)) (spanOf $1) }

--h---- All (non-sum) types not starting with a 'for', '(', or '<'
--h--no_for_ty_prim :: { Ty Span }
--h--  : '_'                              { Infer (spanOf $1) }
--h--  | '!'                              { Never (spanOf $1) }
--h--  | '[' ty ']'                       { Slice $2 ($1 # $3) }
--h--  | '*' ty_no_plus                   { Ptr Immutable $2 ($1 # $2) }
--h--  | '*' const ty_no_plus             { Ptr Immutable $3 ($1 # $3) }
--h--  | '*' mut   ty_no_plus             { Ptr Mutable $3 ($1 # $3) }
--h--  | '&'               ty_no_plus     { Rptr Nothing   Immutable $2 ($1 # $>) }
--h--  | '&'  lifetime     ty_no_plus     { Rptr (Just $2) Immutable $3 ($1 # $>) }
--h--  | '&'           mut ty_no_plus     { Rptr Nothing   Mutable   $3 ($1 # $>) }
--h--  | '&'  lifetime mut ty_no_plus     { Rptr (Just $2) Mutable   $4 ($1 # $>) }
--h--  | '&&'              ty_no_plus     { Rptr Nothing Immutable (Rptr Nothing   Immutable $2 (nudge 1 0 ($1 # $>))) ($1 # $>) }
--h--  | '&&' lifetime     ty_no_plus     { Rptr Nothing Immutable (Rptr (Just $2) Immutable $3 (nudge 1 0 ($1 # $>))) ($1 # $>) }
--h--  | '&&'          mut ty_no_plus     { Rptr Nothing Immutable (Rptr Nothing   Mutable   $3 (nudge 1 0 ($1 # $>))) ($1 # $>) }
--h--  | '&&' lifetime mut ty_no_plus     { Rptr Nothing Immutable (Rptr (Just $2) Mutable   $4 (nudge 1 0 ($1 # $>))) ($1 # $>) }
--h--  | ty_path               %prec PATH { PathTy Nothing $1 ($1 # $>) }
--h--  | ty_mac                           { MacTy $1 ($1 # $>) }
--h--  | unsafe extern abi fn fn_decl(arg_general)     { BareFn Unsafe $3 [] $> ($1 # $>) }
--h--  | unsafe fn fn_decl(arg_general)                { BareFn Unsafe Rust [] $> ($1 # $>) }
--h--  | extern abi fn fn_decl(arg_general)            { BareFn Normal $2 [] $> ($1 # $>) }
--h--  | fn fn_decl(arg_general)                       { BareFn Normal Rust [] $> ($1 # $>) }
--h--  | typeof '(' expr ')'              { Typeof $3 ($1 # $>) }
--h--  | '[' ty ';' expr ']'              { Array $2 $4 ($1 # $>) }
--h--  | '?' trait_ref                    { TraitObject [TraitTyParamBound (PolyTraitRef [] $2 (spanOf $2)) Maybe ($1 # $2)] ($1 # $2) }
--h--  | '?' for_lts trait_ref            { TraitObject [TraitTyParamBound (PolyTraitRef (unspan $2) $3 ($2 # $3)) Maybe ($1 # $3)] ($1 # $3) }
--h--  | impl sep_by1(ty_param_bound_mod,'+') %prec IMPLTRAIT { ImplTrait (toNonEmpty $2) ($1 # $2) }
--h--  | dyn  sep_by1(ty_param_bound_mod,'+') %prec IMPLTRAIT { TraitObject (toNonEmpty $2) ($1 # $2) }
--h--
--h---- All (non-sum) types starting with a 'for'
--h--for_ty_no_plus :: { Ty Span }
--h--  : for_lts unsafe extern abi fn fn_decl(arg_general) { BareFn Unsafe $4 (unspan $1) $> ($1 # $>) }
--h--  | for_lts unsafe fn fn_decl(arg_general)            { BareFn Unsafe Rust (unspan $1) $> ($1 # $>) }
--h--  | for_lts extern abi fn fn_decl(arg_general)        { BareFn Normal $3 (unspan $1) $> ($1 # $>) }
--h--  | for_lts fn fn_decl(arg_general)                   { BareFn Normal Rust (unspan $1) $> ($1 # $>) }
--h--  | for_lts trait_ref                                 {
--h--      let poly = PolyTraitRef (unspan $1) $2 ($1 # $2) in
--h--      TraitObject [TraitTyParamBound poly None ($1 # $2)] ($1 # $2)
--h--    }
--h--
--h---- An optional lifetime followed by an optional mutability
--h--lifetime_mut :: { (Maybe (Lifetime Span), Mutability) }
--h--  : lifetime mut  { (Just $1, Mutable) }
--h--  | lifetime      { (Just $1, Immutable) }
--h--  |          mut  { (Nothing, Mutable) }
--h--  | {- empty -}   { (Nothing, Immutable) }
--h--
--h---- The argument list and return type in a function
--h--fn_decl(arg) :: { FnDecl Span }
--h--  : '(' sep_by1(arg,',') ',' '...' ')' ret_ty  { FnDecl (toList $2) $> True ($1 # $5 # $6) }
--h--  | '(' sep_byT(arg,',')           ')' ret_ty  { FnDecl $2 $> False ($1 # $3 # $4) }
--h--
--h---- Like 'fn_decl', but also accepting a self argument
--h--fn_decl_with_self_general :: { FnDecl Span }
--h--  : '(' arg_self_general ',' sep_byT(arg_general,',') ')' ret_ty  { FnDecl ($2 : $4) $> False ($1 # $5 # $6) }
--h--  | '(' arg_self_general                              ')' ret_ty  { FnDecl [$2] $> False ($1 # $3 # $4) }
--h--  | '('                                               ')' ret_ty  { FnDecl [] $> False ($1 # $2 # $3) }
--h--
--h---- Like 'fn_decl', but also accepting a self argument
--h--fn_decl_with_self_named :: { FnDecl Span }
--h--  : '(' arg_self_named ',' sep_by1(arg_named,',') ',' ')' ret_ty  { FnDecl ($2 : toList $4) $> False ($1 # $6 # $7) }
--h--  | '(' arg_self_named ',' sep_by1(arg_named,',')     ')' ret_ty  { FnDecl ($2 : toList $4) $> False ($1 # $5 # $6) }
--h--  | '(' arg_self_named ','                            ')' ret_ty  { FnDecl [$2] $> False ($1 # $3 # $4) }
--h--  | '(' arg_self_named                                ')' ret_ty  { FnDecl [$2] $> False ($1 # $3 # $4) }
--h--  | fn_decl(arg_named)                                            { $1 }
--h--
--h--
--h---- parse_ty_param_bounds(BoundParsingMode::Bare) == sep_by1(ty_param_bound,'+')
--h--ty_param_bound :: { TyParamBound Span }
--h--  : lifetime                { RegionTyParamBound $1 (spanOf $1) }
--h--  | poly_trait_ref          { TraitTyParamBound $1 None (spanOf $1) }
--h--  | '(' poly_trait_ref ')'  { TraitTyParamBound $2 None ($1 # $3) }
--h--
--h--poly_trait_ref_mod_bound :: { TyParamBound Span }
--h--  : poly_trait_ref       { TraitTyParamBound $1 None (spanOf $1) }
--h--  | '?' poly_trait_ref   { TraitTyParamBound $2 Maybe ($1 # $2) }
--h--
--h---- parse_ty_param_bounds(BoundParsingMode::Modified) == sep_by1(ty_param_bound_mod,'+')
--h--ty_param_bound_mod :: { TyParamBound Span }
--h--  : ty_param_bound       { $1 }
--h--  | '?' poly_trait_ref   { TraitTyParamBound $2 Maybe ($1 # $2) }
--h--
--h---- Sort of like parse_opt_abi() -- currently doesn't handle raw string ABI
--h--abi :: { Abi }
--h--  : str             {% case unspan $1 of
--h--                         LiteralTok (StrTok "cdecl") Nothing ->              pure Cdecl
--h--                         LiteralTok (StrTok "stdcall") Nothing ->            pure Stdcall
--h--                         LiteralTok (StrTok "fastcall") Nothing ->           pure Fastcall
--h--                         LiteralTok (StrTok "vectorcall") Nothing ->         pure Vectorcall
--h--                         LiteralTok (StrTok "aapcs") Nothing ->              pure Aapcs
--h--                         LiteralTok (StrTok "win64") Nothing ->              pure Win64
--h--                         LiteralTok (StrTok "sysv64") Nothing ->             pure SysV64
--h--                         LiteralTok (StrTok "ptx-kernel") Nothing ->         pure PtxKernel
--h--                         LiteralTok (StrTok "msp430-interrupt") Nothing ->   pure Msp430Interrupt
--h--                         LiteralTok (StrTok "x86-interrupt") Nothing ->      pure X86Interrupt
--h--                         LiteralTok (StrTok "Rust") Nothing ->               pure Rust
--h--                         LiteralTok (StrTok "C") Nothing ->                  pure C
--h--                         LiteralTok (StrTok "system") Nothing ->             pure System
--h--                         LiteralTok (StrTok "rust-intrinsic") Nothing ->     pure RustIntrinsic
--h--                         LiteralTok (StrTok "rust-call") Nothing ->          pure RustCall
--h--                         LiteralTok (StrTok "platform-intrinsic") Nothing -> pure PlatformIntrinsic
--h--                         LiteralTok (StrTok "unadjusted") Nothing ->         pure Unadjusted
--h--                         _ -> parseError $1 {- "invalid ABI" -}
--h--                    }
--h--  | {- empty -}     { C }
--h--
--h---- parse_ret_ty
--h--ret_ty :: { Maybe (Ty Span) }
--h--  : '->' ty_no_plus                                  { Just $2 }
--h--  | {- empty -}                                      { Nothing }
--h--
--h---- parse_poly_trait_ref()
--h--poly_trait_ref :: { PolyTraitRef Span }
--h--  :         trait_ref                                { PolyTraitRef [] $1 (spanOf $1) }
--h--  | for_lts trait_ref                                { PolyTraitRef (unspan $1) $2 ($1 # $2) }
--h--
--h---- parse_for_lts()
--h---- Unlike the Rust libsyntax version, this _requires_ the 'for'
--h--for_lts :: { Spanned [LifetimeDef Span] }
--h--  : for '<' sep_byT(lifetime_def,',') '>'            { Spanned $3 ($1 # $>) }
--h--
--h---- Definition of a lifetime: attributes can come before the lifetime, and a list of bounding
--h---- lifetimes can come after the lifetime.
--h--lifetime_def :: { LifetimeDef Span }
--h--  : many(outer_attribute) lifetime ':' sep_by1T(lifetime,'+')  { LifetimeDef $1 $2 (toList $4) ($1 # $2 # $>) }
--h--  | many(outer_attribute) lifetime                             { LifetimeDef $1 $2 [] ($1 # $2 # $>) }
--h--
--h--
--h-----------------
--h---- Arguments --
--h-----------------
--h--
--h---- Argument (requires a name / pattern, ie. @parse_arg_general(true)@)
--h--arg_named :: { Arg Span }
--h--  : ntArg             { $1 }
--h--  | pat ':' ty        { Arg (Just $1) $3 ($1 # $3) }
--h--
--h---- Argument (does not require a name / pattern, ie. @parse_arg_general(false)@)
--h----
--h---- Remark: not all patterns are accepted (as per <https://github.com/rust-lang/rust/issues/35203>)
--h---- The details for which patterns _should_ be accepted fall into @is_named_argument()@.
--h--arg_general :: { Arg Span }
--h--  : ntArg              { $1 }
--h--  |                ty  { Arg Nothing $1 (spanOf $1) }
--h--  |      '_'   ':' ty  { Arg (Just (WildP (spanOf $1))) $3 ($1 # $3) }
--h--  |      ident ':' ty  { Arg (Just (IdentP (ByValue Immutable) (unspan $1) Nothing (spanOf $1))) $3 ($1 # $3) }
--h--  | mut  ident ':' ty  { Arg (Just (IdentP (ByValue Mutable) (unspan $2) Nothing (spanOf $2))) $4 ($1 # $4) }
--h--  | '&'  '_'   ':' ty  { Arg (Just (RefP (WildP (spanOf $2)) Immutable ($1 # $2))) $4 ($1 # $4) }
--h--  | '&'  ident ':' ty  { Arg (Just (RefP (IdentP (ByValue Immutable) (unspan $2) Nothing (spanOf $2)) Immutable ($1 # $2))) $4 ($1 # $4) }
--h--  | '&&' '_'   ':' ty  { Arg (Just (RefP (RefP (WildP (spanOf $2)) Immutable (nudge 1 0 ($1 # $2))) Immutable ($1 # $2))) $4 ($1 # $4) }
--h--  | '&&' ident ':' ty  { Arg (Just (RefP (RefP (IdentP (ByValue Immutable) (unspan $2) Nothing (spanOf $2)) Immutable (nudge 1 0 ($1 # $2))) Immutable ($1 # $2))) $4 ($1 # $4) }
--h--
--h---- Self argument (only allowed in trait function signatures)
--h--arg_self_general :: { Arg Span }
--h--  : mut self              { SelfValue Mutable ($1 # $>) }
--h--  |     self ':' ty       { SelfExplicit $3 Immutable ($1 # $>) }
--h--  | mut self ':' ty       { SelfExplicit $4 Mutable ($1 # $>) }
--h--  | arg_general           {
--h--      case $1 of
--h--        Arg Nothing (PathTy Nothing (Path False [PathSegment "self" Nothing _] _) _) x -> SelfValue Immutable x
--h--        Arg Nothing (Rptr l m (PathTy Nothing (Path False [PathSegment "self" Nothing _] _) _) _) x -> SelfRegion l m x
--h--        _ -> $1
--h--    }
--h--
--h---- Self argument (only allowed in impl function signatures)
--h--arg_self_named :: { Arg Span }
--h--  :                  self { SelfValue Immutable ($1 # $>) }
--h--  |              mut self { SelfValue Mutable ($1 # $>) }
--h--  | '&'              self { SelfRegion Nothing   Immutable ($1 # $>) }
--h--  | '&' lifetime     self { SelfRegion (Just $2) Immutable ($1 # $>) }
--h--  | '&'          mut self { SelfRegion Nothing   Mutable   ($1 # $>) }
--h--  | '&' lifetime mut self { SelfRegion (Just $2) Mutable   ($1 # $>) }
--h--  |     self ':' ty       { SelfExplicit $3 Immutable ($1 # $>) }
--h--  | mut self ':' ty       { SelfExplicit $4 Mutable ($1 # $>) }
--h--
--h---- Lambda expression argument
--h--lambda_arg :: { Arg Span }
--h--  : ntArg                         { $1 }
--h--  | pat ':' ty                    { Arg (Just $1) $3 ($1 # $3) }
--h--  | pat                           { Arg (Just $1) (Infer mempty) (spanOf $1) }
--h--
--h--
--h----------------
--h---- Patterns --
--h----------------
--h--
--h---- There is a funky trick going on here around 'IdentP'. When there is a binding mode (ie a 'mut' or
--h---- 'ref') or an '@' pattern, everything is fine, but otherwise there is no difference between an
--h---- expression variable path and a pattern. To deal with this, we intercept expression paths with
--h---- only one segment, no path parameters, and not global and turn them into identifier patterns.
--h--pat :: { Pat Span }
--h--  : ntPat                           { $1 }
--h--  | '_'                             { WildP (spanOf $1) }
--h--  | '&' mut pat                     { RefP $3 Mutable ($1 # $3) }
--h--  | '&' pat                         { RefP $2 Immutable ($1 # $2) }
--h--  | '&&' mut pat                    { RefP (RefP $3 Mutable (nudge 1 0 ($1 # $3))) Immutable ($1 # $3) }
--h--  | '&&' pat                        { RefP (RefP $2 Immutable (nudge 1 0 ($1 # $2))) Immutable ($1 # $2) }
--h--  |     lit_expr                    { LitP $1 (spanOf $1) }
--h--  | '-' lit_expr                    { LitP (Unary [] Neg $2 ($1 # $2)) ($1 # $2) }
--h--  | box pat                         { BoxP $2 ($1 # $2) }
--h--  | binding_mode1 ident '@' pat     { IdentP (unspan $1) (unspan $2) (Just $4) ($1 # $>) }
--h--  | binding_mode1 ident             { IdentP (unspan $1) (unspan $2) Nothing ($1 # $>) }
--h--  |               ident '@' pat     { IdentP (ByValue Immutable) (unspan $1) (Just $3) ($1 # $>) }
--h--  | expr_path                       {
--h--       case $1 of
--h--         Path False [PathSegment i Nothing _] _ -> IdentP (ByValue Immutable) i Nothing (spanOf $1)
--h--         _                                      -> PathP Nothing $1 (spanOf $1)
--h--    }
--h--  | expr_qual_path                  { PathP (Just (fst (unspan $1))) (snd (unspan $1)) ($1 # $>) }
--h--  | lit_or_path '...' lit_or_path   { RangeP $1 $3 ($1 # $>) }
--h--  | lit_or_path '..=' lit_or_path   { RangeP $1 $3 ($1 # $>) }
--h--  | expr_path '{' '..' '}'          { StructP $1 [] True ($1 # $>) }
--h--  | expr_path '{' pat_fields '}'    { let (fs,b) = $3 in StructP $1 fs b ($1 # $>) }
--h--  | expr_path '(' pat_tup ')'       { let (ps,m,_) = $3 in TupleStructP $1 ps m ($1 # $>) }
--h--  | expr_mac                        { MacP $1 (spanOf $1) }
--h--  | '[' pat_slice ']'               { let (b,s,a) = $2 in SliceP b s a ($1 # $3) }
--h--  | '(' pat_tup ')'                 {%
--h--      case $2 of
--h--        ([p], Nothing, False) -> parseError (CloseDelim Paren)
--h--        (ps,m,t) -> pure (TupleP ps m ($1 # $3))
--h--    }
--h--
--h--
--h---- The first element is the spans, the second the position of '..', and the third if there is a
--h---- trailing comma
--h--pat_tup :: { ([Pat Span], Maybe Int, Bool) }
--h--  : sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',')     { (toList ($1 <> $5), Just (length $1), False) }
--h--  | sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',') ',' { (toList ($1 <> $5), Just (length $1), True) }
--h--  | sep_by1(pat,',') ',' '..'                          { (toList $1,         Just (length $1), False) }
--h--  | sep_by1(pat,',')                                   { (toList $1,         Nothing,          False) }
--h--  | sep_by1(pat,',') ','                               { (toList $1,         Nothing,          True) }
--h--  |                      '..' ',' sep_by1(pat,',')     { (toList $3,         Just 0,           False) }
--h--  |                      '..' ',' sep_by1(pat,',') ',' { (toList $3,         Just 0,           True) }
--h--  |                      '..'                          { ([],                Just 0,           False) }
--h--  | {- empty -}                                        { ([],                Nothing,          False) }
--h--
--h---- The first element is the patterns at the beginning of the slice, the second the optional binding
--h---- for the middle slice ('Nothing' if there is no '..' and 'Just (WildP mempty) is there is one, but
--h---- unlabelled), and the third is the patterns at the end of the slice.
--h--pat_slice :: { ([Pat Span], Maybe (Pat Span), [Pat Span]) }
--h--  : sep_by1(pat,',') ',' '..' ',' sep_by1T(pat,',')    { (toList $1, Just (WildP mempty), toList $5) }
--h--  | sep_by1(pat,',') ',' '..'                          { (toList $1, Just (WildP mempty), []) }
--h--  | sep_by1(pat,',')     '..' ',' sep_by1T(pat,',')    { let (xs, x) = unsnoc $1 in (toList xs, Just x,    toList $4) }
--h--  | sep_by1(pat,',')     '..'                          { let (xs, x) = unsnoc $1 in (toList xs, Just x,    []) }
--h--  |                               sep_by1T(pat,',')    { (toList $1, Nothing,             []) }
--h--  |                      '..' ',' sep_by1T(pat,',')    { ([],        Just (WildP mempty), toList $3) }
--h--  |                      '..'                          { ([],        Just (WildP mempty), []) }
--h--  | {- empty -}                                        { ([],        Nothing,             []) }
--h--
--h--
--h---- Endpoints of range patterns
--h--lit_or_path :: { Expr Span }
--h--  : expr_path         { PathExpr [] Nothing $1 (spanOf $1) }
--h--  | expr_qual_path    { PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1)) (spanOf $1) }
--h--  | '-' lit_expr      { Unary [] Neg $2 ($1 # $2) }
--h--  |     lit_expr      { $1 }
--h--
--h---- Used in patterns for tuple and expression patterns
--h--pat_fields :: { ([FieldPat Span], Bool) }
--h--  : sep_byT(pat_field,',')           { ($1, False) }
--h--  | sep_by1(pat_field,',') ',' '..'  { (toList $1, True) }
--h--
--h--pat_field :: { FieldPat Span }
--h--  :     binding_mode ident
--h--    { FieldPat Nothing (IdentP (unspan $1) (unspan $2) Nothing (spanOf $2)) ($1 # $2) }
--h--  | box binding_mode ident
--h--    { FieldPat Nothing (BoxP (IdentP (unspan $2) (unspan $3) Nothing ($2 # $3)) ($1 # $3)) ($1 # $3) }
--h--  |     binding_mode ident ':' pat
--h--    { FieldPat (Just (unspan $2)) $4 ($1 # $2 # $4) }
--h--
--h--
--h---- Used prefixing IdentP patterns (not empty - that is a seperate pattern case)
--h--binding_mode1 :: { Spanned BindingMode }
--h--  : ref mut                          { Spanned (ByRef Mutable) ($1 # $2) }
--h--  | ref                              { Spanned (ByRef Immutable) (spanOf $1) }
--h--  |     mut                          { Spanned (ByValue Mutable) (spanOf $1) }
--h--
--h---- Used for patterns for fields (includes the empty case)
--h--binding_mode :: { Spanned BindingMode }
--h--  : binding_mode1                    { $1 }
--h--  | {- empty -}                      { Spanned (ByValue Immutable) mempty }
--h--
--h--
--h-------------------
--h---- Expressions --
--h-------------------
--h--
--h---- Expressions are a pain to parse. The Rust language places "restrictions" preventing certain
--h---- specific expressions from being valid in a certain context. Elsewhere in the parser, it will turn
--h---- on or off these restrictions. Unfortunately, that doesn't work well at all in a grammar, so we
--h---- have to define production rules for every combination of restrications used. Parametrized
--h---- productions make this a bit easier by letting us factor out the core expressions used everywhere.
--h--
--h---- Generalized expressions, parametrized by
--h----
--h----   * 'lhs' - expressions allowed on the left extremity of the term
--h----   * 'rhs' - expressions allowed on the right extremity of the term
--h----   * 'rhs2' - expressions allowed on the right extremity following '..'/'.../..='
--h----
--h---- Precedences are handled by Happy (right at the end of the token section)
--h--gen_expression(lhs,rhs,rhs2) :: { Expr Span }
--h--  -- immediate expressions
--h--  : ntExpr                           { $1 }
--h--  | lit_expr                         { $1 }
--h--  | '[' sep_byT(expr,',') ']'        { Vec [] $2 ($1 # $>) }
--h--  | '[' inner_attrs sep_byT(expr,',') ']' { Vec (toList $2) $3 ($1 # $>) }
--h--  | '[' expr ';' expr ']'            { Repeat [] $2 $4 ($1 # $>) }
--h--  | expr_mac                         { MacExpr [] $1 (spanOf $1) }
--h--  | expr_path            %prec PATH  { PathExpr [] Nothing $1 (spanOf $1) }
--h--  | expr_qual_path                   { PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1)) (spanOf $1) }
--h--  -- unary expressions
--h--  | '*'      rhs     %prec UNARY     { Unary [] Deref $2 ($1 # $>) }
--h--  | '!'      rhs     %prec UNARY     { Unary [] Not $2 ($1 # $>) }
--h--  | '-'      rhs     %prec UNARY     { Unary [] Neg $2 ($1 # $>) }
--h--  | '&'      rhs     %prec UNARY     { AddrOf [] Immutable $2 ($1 # $>) }
--h--  | '&'  mut rhs     %prec UNARY     { AddrOf [] Mutable $3 ($1 # $>) }
--h--  | '&&'     rhs     %prec UNARY     { AddrOf [] Immutable (AddrOf [] Immutable $2 (nudge 1 0 ($1 # $2))) ($1 # $2) }
--h--  | '&&' mut rhs     %prec UNARY     { AddrOf [] Immutable (AddrOf [] Mutable $3 (nudge 1 0 ($1 # $3))) ($1 # $3) }
--h--  | box rhs          %prec UNARY     { Box [] $2 ($1 # $>) }
--h--  -- left-recursive
--h--  | left_gen_expression(lhs,rhs,rhs2) { $1 }
--h--  -- range expressions
--h--  |     '..'  rhs2  %prec PREFIXRNG  { Range [] Nothing (Just $2) HalfOpen ($1 # $2) }
--h--  |     '...' rhs2  %prec PREFIXRNG  { Range [] Nothing (Just $2) Closed ($1 # $2) }
--h--  |     '..=' rhs2  %prec PREFIXRNG  { Range [] Nothing (Just $2) Closed ($1 # $2) }
--h--  |     '..'        %prec SINGLERNG  { Range [] Nothing Nothing HalfOpen (spanOf $1) }
--h--  |     '..='       %prec SINGLERNG  { Range [] Nothing Nothing Closed (spanOf $1) }
--h--  -- low precedence prefix expressions
--h--  | return                           { Ret [] Nothing (spanOf $1) }
--h--  | return rhs                       { Ret [] (Just $2) ($1 # $2) }
--h--  | yield                            { Yield [] Nothing (spanOf $1) }
--h--  | yield rhs                        { Yield [] (Just $2) ($1 # $2) }
--h--  | continue                         { Continue [] Nothing (spanOf $1) }
--h--  | continue label                   { Continue [] (Just $2) ($1 # $2) }
--h--  | break                            { Break [] Nothing Nothing (spanOf $1) }
--h--  | break       rhs                  { Break [] Nothing (Just $2) ($1 # $2) }
--h--  | break label                      { Break [] (Just $2) Nothing ($1 # $2) }
--h--  | break label rhs      %prec break { Break [] (Just $2) (Just $3) ($1 # $3) }
--h--  -- lambda expressions
--h--  | static move lambda_args rhs   %prec LAMBDA
--h--    { Closure [] Immovable Value (FnDecl (unspan $3) Nothing False (spanOf $3)) $> ($1 # $>) }
--h--  |        move lambda_args rhs   %prec LAMBDA
--h--    { Closure [] Movable Value (FnDecl (unspan $2) Nothing False (spanOf $2)) $> ($1 # $>) }
--h--  | static      lambda_args rhs   %prec LAMBDA
--h--    { Closure [] Immovable Ref   (FnDecl (unspan $2) Nothing False (spanOf $2)) $> ($1 # $>) }
--h--  |             lambda_args rhs   %prec LAMBDA
--h--    { Closure [] Movable Ref   (FnDecl (unspan $1) Nothing False (spanOf $1)) $> ($1 # $>) }
--h--
--h---- Variant of 'gen_expression' which only constructs expressions starting with another expression.
--h--left_gen_expression(lhs,rhs,rhs2) :: { Expr Span }
--h--  : postfix_blockexpr(lhs)           { $1 }
--h--  | lhs '[' expr ']'                 { Index [] $1 $3 ($1 # $>) }
--h--  | lhs '(' sep_byT(expr,',') ')'    { Call [] $1 $3 ($1 # $>) }
--h--  -- unary expressions
--h--  | lhs ':' ty_no_plus               { TypeAscription [] $1 $3 ($1 # $>) }
--h--  | lhs as ty_no_plus                { Cast [] $1 $3 ($1 # $>) }
--h--  -- binary expressions
--h--  | lhs '*' rhs                      { Binary [] MulOp $1 $3 ($1 # $>) }
--h--  | lhs '/' rhs                      { Binary [] DivOp $1 $3 ($1 # $>) }
--h--  | lhs '%' rhs                      { Binary [] RemOp $1 $3 ($1 # $>) }
--h--  | lhs '+' rhs                      { Binary [] AddOp $1 $3 ($1 # $>) }
--h--  | lhs '-' rhs                      { Binary [] SubOp $1 $3 ($1 # $>) }
--h--  | lhs '<<' rhs                     { Binary [] ShlOp $1 $3 ($1 # $>) }
--h--  | lhs '>>' rhs                     { Binary [] ShrOp $1 $3 ($1 # $>) }
--h--  | lhs '&' rhs                      { Binary [] BitAndOp $1 $3 ($1 # $>) }
--h--  | lhs '^' rhs                      { Binary [] BitXorOp $1 $3 ($1 # $>) }
--h--  | lhs '|' rhs                      { Binary [] BitOrOp $1 $3 ($1 # $>) }
--h--  | lhs '==' rhs                     { Binary [] EqOp $1 $3 ($1 # $>) }
--h--  | lhs '!=' rhs                     { Binary [] NeOp $1 $3 ($1 # $>) }
--h--  | lhs '<'  rhs                     { Binary [] LtOp $1 $3 ($1 # $>) }
--h--  | lhs '>'  rhs                     { Binary [] GtOp $1 $3 ($1 # $>) }
--h--  | lhs '<=' rhs                     { Binary [] LeOp $1 $3 ($1 # $>) }
--h--  | lhs '>=' rhs                     { Binary [] GeOp $1 $3 ($1 # $>) }
--h--  | lhs '&&' rhs                     { Binary [] AndOp $1 $3 ($1 # $>) }
--h--  | lhs '||' rhs                     { Binary [] OrOp $1 $3 ($1 # $>) }
--h--  -- range expressions
--h--  | lhs '..'        %prec POSTFIXRNG { Range [] (Just $1) Nothing HalfOpen ($1 # $>) }
--h--  | lhs '...'       %prec POSTFIXRNG { Range [] (Just $1) Nothing Closed ($1 # $>) }
--h--  | lhs '..='       %prec POSTFIXRNG { Range [] (Just $1) Nothing Closed ($1 # $>) }
--h--  | lhs '..'  rhs2  %prec INFIXRNG   { Range [] (Just $1) (Just $3) HalfOpen ($1 # $>) }
--h--  | lhs '...' rhs2  %prec INFIXRNG   { Range [] (Just $1) (Just $3) Closed ($1 # $>) }
--h--  | lhs '..=' rhs2  %prec INFIXRNG   { Range [] (Just $1) (Just $3) Closed ($1 # $>) }
--h--  -- assignment expressions
--h--  | lhs '<-' rhs                     { InPlace [] $1 $3 ($1 # $>) }
--h--  | lhs '=' rhs                      { Assign [] $1 $3 ($1 # $>) }
--h--  | lhs '>>=' rhs                    { AssignOp [] ShrOp $1 $3 ($1 # $>) }
--h--  | lhs '<<=' rhs                    { AssignOp [] ShlOp $1 $3 ($1 # $>) }
--h--  | lhs '-=' rhs                     { AssignOp [] SubOp $1 $3 ($1 # $>) }
--h--  | lhs '+=' rhs                     { AssignOp [] AddOp $1 $3 ($1 # $>) }
--h--  | lhs '*=' rhs                     { AssignOp [] MulOp $1 $3 ($1 # $>) }
--h--  | lhs '/=' rhs                     { AssignOp [] DivOp $1 $3 ($1 # $>) }
--h--  | lhs '^=' rhs                     { AssignOp [] BitXorOp $1 $3 ($1 # $>) }
--h--  | lhs '|=' rhs                     { AssignOp [] BitOrOp $1 $3 ($1 # $>) }
--h--  | lhs '&=' rhs                     { AssignOp [] BitAndOp $1 $3 ($1 # $>) }
--h--  | lhs '%=' rhs                     { AssignOp [] RemOp $1 $3 ($1 # $>) }
--h--
--h---- Postfix expressions that can come after an expression block, in a 'stmt'
--h----
--h----  * `{ 1 }[0]` isn't here because it is treated as `{ 1 }; [0]`
--h----  * `{ 1 }(0)` isn't here because it is treated as `{ 1 }; (0)`
--h----
--h--postfix_blockexpr(lhs) :: { Expr Span }
--h--  : lhs '?'                          { Try [] $1 ($1 # $>) }
--h--  | lhs '.' ident       %prec FIELD  { FieldAccess [] $1 (unspan $3) ($1 # $>) }
--h--  | lhs '.' ident '(' sep_byT(expr,',') ')'
--h--    { MethodCall [] $1 (unspan $3) Nothing $5 ($1 # $>) }
--h--  | lhs '.' ident '::' '<' sep_byT(ty,',') '>' '(' sep_byT(expr,',') ')'
--h--    { MethodCall [] $1 (unspan $3) (Just $6) $9 ($1 # $>) }
--h--  | lhs '.' int                      {%
--h--      case lit $3 of
--h--        Int Dec i Unsuffixed _ -> pure (TupField [] $1 (fromIntegral i) ($1 # $3))
--h--        _ -> parseError $3
--h--    }
--h--
--h---- Postfix expressions that can come after an expression block, in a 'stmt'
--h----
--h----  * `{ 1 }[0]` isn't here because it is treated as `{ 1 }; [0]`
--h----  * `{ 1 }(0)` isn't here because it is treated as `{ 1 }; (0)`
--h----
--h--postfix_blockexpr(lhs) :: { Expr Span }
--h--  : lhs '?'                          { Try [] $1 ($1 # $>) }
--h--  | lhs '.' ident       %prec FIELD  { FieldAccess [] $1 (unspan $3) ($1 # $>) }
--h--  | lhs '.' ident '(' sep_byT(expr,',') ')'
--h--    { MethodCall [] $1 (unspan $3) Nothing $5 ($1 # $>) }
--h--  | lhs '.' ident '::' '<' sep_byT(ty,',') '>' '(' sep_byT(expr,',') ')'
--h--    { MethodCall [] $1 (unspan $3) (Just $6) $9 ($1 # $>) }
--h--  | lhs '.' int                      {%
--h--      case lit $3 of
--h--        Int Dec i Unsuffixed _ -> pure (TupField [] $1 (fromIntegral i) ($1 # $3))
--h--        _ -> parseError $3
--h--    }
--h--
--h---- Postfix expressions that can come after an expression block, in a 'stmt'
--h----
--h----  * `{ 1 }[0]` isn't here because it is treated as `{ 1 }; [0]`
--h----  * `{ 1 }(0)` isn't here because it is treated as `{ 1 }; (0)`
--h----
--h--postfix_blockexpr(lhs) :: { Expr Span }
--h--  : lhs '?'                          { Try [] $1 ($1 # $>) }
--h--  | lhs '.' ident       %prec FIELD  { FieldAccess [] $1 (unspan $3) ($1 # $>) }
--h--  | lhs '.' ident '(' sep_byT(expr,',') ')'
--h--    { MethodCall [] $1 (unspan $3) Nothing $5 ($1 # $>) }
--h--  | lhs '.' ident '::' '<' sep_byT(ty,',') '>' '(' sep_byT(expr,',') ')'
--h--    { MethodCall [] $1 (unspan $3) (Just $6) $9 ($1 # $>) }
--h--  | lhs '.' int                      {%
--h--      case lit $3 of
--h--        Int Dec i Unsuffixed _ -> pure (TupField [] $1 (fromIntegral i) ($1 # $3))
--h--        _ -> parseError $3
--h--    }
--h--
--h--
--h--
--h---- Then, we instantiate this general production into the following families of rules:
--h----
--h----   ['expr']               Most general class of expressions, no restrictions
--h----
--h----   ['nostruct_expr']      Forbids struct literals (for use as scrutinee of loops, ifs, etc)
--h----
--h----   ['nostructblock_expr'] Forbids struct literals and block expressions (but not block-like things
--h----                          like 'if' expressions or 'loop' expressions)
--h----
--h----   ['nonblock_expr']      Forbids expressions starting with blocks (things such as '{ 1 } + 2' are
--h----                          not allowed, while struct expressions are - their "block" is at the end
--h----                          of the expression)
--h----
--h----   ['blockpostfix_expr']  Allows expressions starting with blocks (things such as '{ 1 }? + 1')
--h----                          but only when the leading block is itself a postfix expression.
--h----
--h---- There is also a later instantiation revolving around 'match' expressions, but it has some
--h---- different types.
--h--
--h--expr :: { Expr Span }
--h--  : gen_expression(expr,expr,expr)                                            { $1 }
--h--  | paren_expr                                                                { $1 }
--h--  | struct_expr                                                               { $1 }
--h--  | block_expr                                                                { $1 }
--h--  | lambda_expr_block                                                         { $1 }
--h--
--h--nostruct_expr :: { Expr Span }
--h--  : gen_expression(nostruct_expr,nostruct_expr,nonstructblock_expr)           { $1 }
--h--  | paren_expr                                                                { $1 }
--h--  | block_expr                                                                { $1 }
--h--
--h--nonstructblock_expr :: { Expr Span }
--h--  : gen_expression(nonstructblock_expr,nostruct_expr,nonstructblock_expr)     { $1 }
--h--  | paren_expr                                                                { $1 }
--h--  | block_like_expr                                                           { $1 }
--h--  | unsafe inner_attrs_block
--h--    { let (as, Block ss r x) = $> in BlockExpr as (Block ss Unsafe ($1 # x)) ($1 # x) }
--h--
--h--nonblock_expr :: { Expr Span }
--h--  : gen_expression(nonblock_expr,expr,expr)                                   { $1 }
--h--  | paren_expr                                                                { $1 }
--h--  | struct_expr                                                               { $1 }
--h--  | lambda_expr_block                                                         { $1 }
--h--
--h--blockpostfix_expr :: { Expr Span }
--h--  : postfix_blockexpr(block_like_expr)                                        { $1 }
--h--  | postfix_blockexpr(vis_safety_block)                                       { $1 }
--h--  | left_gen_expression(blockpostfix_expr,expr,expr)                          { $1 }
--h--
--h--
--h---- Finally, what remains is the more mundane definitions of particular types of expressions.
--h--
--h---- labels on loops
--h--label :: { Label Span }
--h--  : LIFETIME                         { let Spanned (LifetimeTok (Ident l _ _)) s = $1 in Label l s }
--h--
--h---- Literal expressions (composed of just literals)
--h--lit_expr :: { Expr Span }
--h--  : lit                                                 { Lit [] $1 (spanOf $1) }
--h--
--h---- An expression ending in a '{ ... }' block. Useful since "There is a convenience rule that allows
--h---- one to omit the separating ';' after 'if', 'match', 'loop', 'for', 'while'"
--h--block_expr :: { Expr Span }
--h--  : block_like_expr                                     { $1 }
--h--  | inner_attrs_block                                   { let (as,b) = $1 in BlockExpr as b (spanOf b) }
--h--  | unsafe inner_attrs_block
--h--    { let (as, Block ss r x) = $> in BlockExpr as (Block ss Unsafe ($1 # x)) ($1 # x) }
--h--
--h---- Any expression ending in a '{ ... }' block except a block itself.
--h--block_like_expr :: { Expr Span }
--h--  : if_expr                                                      { $1 }
--h--  |           loop                            inner_attrs_block  { let (as,b) = $> in Loop as b Nothing ($1 # b) }
--h--  | label ':' loop                            inner_attrs_block  { let (as,b) = $> in Loop as b (Just $1) ($1 # b) }
--h--  |           for pat in nostruct_expr        inner_attrs_block  { let (as,b) = $> in ForLoop as $2 $4 b Nothing ($1 # b) }
--h--  | label ':' for pat in nostruct_expr        inner_attrs_block  { let (as,b) = $> in ForLoop as $4 $6 b (Just $1) ($1 # b) }
--h--  |           while             nostruct_expr inner_attrs_block  { let (as,b) = $> in While as $2 b Nothing ($1 # b) }
--h--  | label ':' while             nostruct_expr inner_attrs_block  { let (as,b) = $> in While as $4 b (Just $1) ($1 # b) }
--h--  |           while let pats '=' nostruct_expr inner_attrs_block { let (as,b) = $> in WhileLet as $3 $5 b Nothing ($1 # b) }
--h--  | label ':' while let pats '=' nostruct_expr inner_attrs_block { let (as,b) = $> in WhileLet as $5 $7 b (Just $1) ($1 # b) }
--h--  | match nostruct_expr '{'                  '}'                 { Match [] $2 [] ($1 # $>) }
--h--  | match nostruct_expr '{' inner_attrs      '}'                 { Match (toList $4) $2 [] ($1 # $>) }
--h--  | match nostruct_expr '{'             arms '}'                 { Match [] $2 $4 ($1 # $>) }
--h--  | match nostruct_expr '{' inner_attrs arms '}'                 { Match (toList $4) $2 $5 ($1 # $>) }
--h--  | expr_path '!' '{' token_stream '}'                           { MacExpr [] (Mac $1 $4 ($1 # $>)) ($1 # $>) }
--h--  | do catch inner_attrs_block                                   { let (as,b) = $> in Catch as b ($1 # b) }
--h--
--h---- 'if' expressions are a bit special since they can have an arbitrary number of 'else if' chains.
--h--if_expr :: { Expr Span }
--h--  : if             nostruct_expr block else_expr        { If [] $2 $3 $4 ($1 # $3 # $>) }
--h--  | if let pats '=' nostruct_expr block else_expr       { IfLet [] $3 $5 $6 $7 ($1 # $6 # $>) }
--h--
--h--else_expr :: { Maybe (Expr Span) }
--h--  : else block                                          { Just (BlockExpr [] $2 (spanOf $2)) }
--h--  | else if_expr                                        { Just $2 }
--h--  | {- empty -}                                         { Nothing }
--h--
--h---- Match arms usually have to be seperated by commas (with an optional comma at the end). This
--h---- condition is loosened (so that there is no seperator needed) if the arm ends in a safe block.
--h--arms :: { [Arm Span] }
--h--  : ntArm                                               { [$1] }
--h--  | ntArm arms                                          { $1 : $2 }
--h--  | many(outer_attribute) pats arm_guard '=>' expr_arms { let (e,as) = $> in (Arm $1 $2 $3 e ($1 # $2 # e) : as) }
--h--
--h--pats :: { NonEmpty (Pat Span) }
--h--  : '|' sep_by1(pat,'|')   { toNonEmpty $2 }
--h--  |     sep_by1(pat,'|')   { toNonEmpty $1 }
--h--
--h--arm_guard :: { Maybe (Expr Span) }
--h--  : {- empty -}  { Nothing }
--h--  | if expr      { Just $2 }
--h--
--h---- Possibly more match arms, with a comma if present
--h--comma_arms :: { [Arm Span] }
--h--  : {- empty -}  { [] }
--h--  | ','          { [] }
--h--  | ',' arms     { $2 }
--h--
--h---- An expression followed by match arms. If there is a comma needed, it is added
--h--expr_arms :: { (Expr Span, [Arm Span]) }
--h--  : nonblock_expr                           comma_arms  { ($1, $2) }
--h--  | blockpostfix_expr                       comma_arms  { ($1, $2) }
--h--  | vis_safety_block                        comma_arms  { ($1, $2) }
--h--  | vis_safety_block                              arms  { ($1, $2) }
--h--  | block_like_expr                         comma_arms  { ($1, $2) }
--h--  | block_like_expr                               arms  { ($1, $2) }
--h--
--h---- As per https://github.com/rust-lang/rust/issues/15701 (as of March 10 2017), the only way to have
--h---- attributes on expressions should be with inner attributes on a paren expression.
--h--paren_expr :: { Expr Span }
--h--  : '(' ')'                                             { TupExpr [] [] ($1 # $>) }
--h--  | '(' inner_attrs ')'                                 { TupExpr (toList $2) [] ($1 # $>) }
--h--  | '('             expr ')'                            { ParenExpr [] $2 ($1 # $>) }
--h--  | '(' inner_attrs expr ')'                            { ParenExpr (toList $2) $3 ($1 # $>) }
--h--  | '('             expr ',' ')'                        { TupExpr [] [$2] ($1 # $>) }
--h--  | '(' inner_attrs expr ',' ')'                        { TupExpr (toList $2) [$3] ($1 # $>) }
--h--  | '('             expr ',' sep_by1T(expr,',') ')'     { TupExpr [] ($2 : toList $4) ($1 # $>) }
--h--  | '(' inner_attrs expr ',' sep_by1T(expr,',') ')'     { TupExpr (toList $2) ($3 : toList $5) ($1 # $>) }
--h--
--h--
--h---- Closure ending in blocks
--h--lambda_expr_block :: { Expr Span }
--h--  : static move lambda_args '->' ty_no_plus block
--h--    { Closure [] Immovable Value (FnDecl (unspan $3) (Just $5) False (spanOf $3)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }
--h--  |        move lambda_args '->' ty_no_plus block
--h--    { Closure [] Movable Value (FnDecl (unspan $2) (Just $4) False (spanOf $2)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }
--h--  | static      lambda_args '->' ty_no_plus block
--h--    { Closure [] Immovable Ref   (FnDecl (unspan $2) (Just $4) False (spanOf $2)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }
--h--  |             lambda_args '->' ty_no_plus block
--h--    { Closure [] Movable Ref   (FnDecl (unspan $1) (Just $3) False (spanOf $1)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }
--h--
--h---- Lambda expression arguments block
--h--lambda_args :: { Spanned [Arg Span] }
--h--  : '||'                                                { Spanned [] (spanOf $1) }
--h--  | '|' sep_byT(lambda_arg,',') pipe '|'                { Spanned $2 ($1 # $4) }
--h--
--h--
--h---- Struct expression literal
--h--struct_expr :: { Expr Span }
--h--  : expr_path '{'                                    '..' expr '}'  { Struct [] $1 [] (Just $4) ($1 # $>) }
--h--  | expr_path '{' inner_attrs                        '..' expr '}'  { Struct (toList $3) $1 [] (Just $5) ($1 # $>) }
--h--  | expr_path '{'             sep_by1(field,',') ',' '..' expr '}'  { Struct [] $1 (toList $3) (Just $6) ($1 # $>) }
--h--  | expr_path '{' inner_attrs sep_by1(field,',') ',' '..' expr '}'  { Struct (toList $3) $1 (toList $4) (Just $7) ($1 # $>) }
--h--  | expr_path '{'             sep_byT(field,',')               '}'  { Struct [] $1 $3 Nothing ($1 # $>) }
--h--  | expr_path '{' inner_attrs sep_byT(field,',')               '}'  { Struct (toList $3) $1 $4 Nothing ($1 # $>) }
--h--
--h--field :: { Field Span }
--h--  : ident ':' expr  { Field (unspan $1) (Just $3) ($1 # $3) }
--h--  | ident           { Field (unspan $1) Nothing (spanOf $1) }
--h--
--h---- an expression block that won't cause conflicts with stmts
--h--vis_safety_block :: { Expr Span }
--h--  : pub_or_inherited safety inner_attrs_block {%
--h--       let (as, Block ss r x) = $3
--h--           e = BlockExpr as (Block ss (unspan $2) ($2 # x)) ($2 # x)
--h--       in noVis $1 e
--h--    }
--h--
--h---- an expression starting with 'union' or 'default' (as identifiers) that won't cause conflicts with stmts
--h--vis_union_def_nonblock_expr :: { Expr Span }
--h--  : union_default_expr                                               { $1 }
--h--  | left_gen_expression(vis_union_def_nonblock_expr, expr, expr) { $1 }
--h--
--h--union_default_expr :: { Expr Span }
--h--  : pub_or_inherited union         {%
--h--      noVis $1 (PathExpr [] Nothing (Path False [PathSegment "union" Nothing (spanOf $2)] (spanOf $1)) (spanOf $1))
--h--    }
--h--  | pub_or_inherited default         {%
--h--      noVis $1 (PathExpr [] Nothing (Path False [PathSegment "default" Nothing (spanOf $2)] (spanOf $1)) (spanOf $1))
--h--    }
--h--
--h--
--h------------------
--h---- Statements --
--h------------------
--h--
--h--stmt :: { Stmt Span }
--h--  : ntStmt                                                 { $1 }
--h--  | many(outer_attribute) let pat ':' ty initializer ';'   { Local $3 (Just $5) $6 $1 ($1 # $2 # $>) }
--h--  | many(outer_attribute) let pat        initializer ';'   { Local $3 Nothing $4 $1 ($1 # $2 # $>) }
--h--  | many(outer_attribute) nonblock_expr ';'                { toStmt ($1 `addAttrs` $2) True  False ($1 # $2 # $3) }
--h--  | many(outer_attribute) block_like_expr ';'              { toStmt ($1 `addAttrs` $2) True  True  ($1 # $2 # $3) }
--h--  | many(outer_attribute) blockpostfix_expr ';'            { toStmt ($1 `addAttrs` $2) True  True  ($1 # $2 # $3) }
--h--  | many(outer_attribute) vis_union_def_nonblock_expr ';'  { toStmt ($1 `addAttrs` $2) True  False ($1 # $2 # $3) }
--h--  | many(outer_attribute) block_like_expr    %prec NOSEMI  { toStmt ($1 `addAttrs` $2) False True  ($1 # $2) }
--h--  | many(outer_attribute) vis_safety_block ';'             { toStmt ($1 `addAttrs` $2) True True ($1 # $2 # $>) }
--h--  | many(outer_attribute) vis_safety_block   %prec NOSEMI  { toStmt ($1 `addAttrs` $2) False True ($1 # $2) }
--h--  | gen_item(pub_or_inherited)                             { ItemStmt $1 (spanOf $1) }
--h--  | many(outer_attribute) expr_path '!' ident '[' token_stream ']' ';'
--h--    { ItemStmt (macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>)) ($1 # $2 # $>) }
--h--  | many(outer_attribute) expr_path '!' ident '(' token_stream ')' ';'
--h--    { ItemStmt (macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>)) ($1 # $2 # $>) }
--h--  | many(outer_attribute) expr_path '!' ident '{' token_stream '}'
--h--    { ItemStmt (macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>)) ($1 # $2 # $>) }
--h--
--h--pub_or_inherited :: { Spanned (Visibility Span) }
--h--  : pub                                          %prec VIS { Spanned PublicV (spanOf $1) }
--h--  | {- empty -}                                  %prec VIS { pure InheritedV }
--h--
--h--stmtOrSemi :: { Maybe (Stmt Span) }
--h--  : ';'                                                    { Nothing }
--h--  | stmt                                                   { Just $1 }
--h--
--h---- List of statements where the last statement might be a no-semicolon statement.
--h--stmts_possibly_no_semi :: { [Maybe (Stmt Span)] }
--h--  : stmtOrSemi stmts_possibly_no_semi                      { $1 : $2 }
--h--  | stmtOrSemi                                             { [$1] }
--h--  | many(outer_attribute) nonblock_expr                    { [Just (toStmt ($1 `addAttrs` $2) False False ($1 # $2))] }
--h--  | many(outer_attribute) blockpostfix_expr                { [Just (toStmt ($1 `addAttrs` $2) False True  ($1 # $2))] }
--h--
--h--initializer :: { Maybe (Expr Span) }
--h--  : '=' expr                                               { Just $2 }
--h--  | {- empty -}                                            { Nothing }
--h--
--h--block :: { Block Span }
--h--  : ntBlock                                                { $1 }
--h--  | '{' '}'                                                { Block [] Normal ($1 # $>) }
--h--  | '{' stmts_possibly_no_semi '}'                         { Block [ s | Just s <- $2 ] Normal ($1 # $>) }
--h--
--h--inner_attrs_block :: { ([Attribute Span], Block Span) }
--h--  : block                                                  { ([], $1) }
--h--  | '{' inner_attrs '}'                                    { (toList $2, Block [] Normal ($1 # $>)) }
--h--  | '{' inner_attrs stmts_possibly_no_semi '}'             { (toList $2, Block [ s | Just s <- $3 ] Normal ($1 # $>)) }
--h--
--h--
--h-------------
--h---- Items --
--h-------------
--h--
--h---- Given the types of permitted visibilities, generate a rule for items. The reason this production
--h---- is useful over just having 'item :: { ItemSpan }' and then 'many(outer_attribute) vis item' is
--h---- that (1) not all items have visibility and (2) attributes and visibility are fields on the 'Item'
--h---- algebraic data type.
--h--gen_item(vis) :: { Item Span }
--h--  : many(outer_attribute) vis static     ident ':' ty '=' expr ';'
--h--    { Static $1 (unspan $2) (unspan $4) $6 Immutable $8 ($1 # $2 # $3 # $>) }
--h--  | many(outer_attribute) vis static mut ident ':' ty '=' expr ';'
--h--    { Static $1 (unspan $2) (unspan $5) $7 Mutable $9 ($1 # $2 # $3 # $>) }
--h--  | many(outer_attribute) vis const ident ':' ty '=' expr ';'
--h--    { ConstItem $1 (unspan $2) (unspan $4) $6 $8 ($1 # $2 # $3 # $>) }
--h--  | many(outer_attribute) vis type ident generics where_clause '=' ty ';'
--h--    { TyAlias $1 (unspan $2) (unspan $4) $8 ($5 `withWhere` $6) ($1 # $2 # $3 # $>) }
--h--  | many(outer_attribute) vis use use_tree ';'
--h--    { Use $1 (unspan $2) $4 ($1 # $2 # $3 # $>) }
--h--  | many(outer_attribute) vis safety extern crate ident ';'
--h--    {% noSafety $3 (ExternCrate $1 (unspan $2) (unspan $6) Nothing ($1 # $2 # $4 # $>)) }
--h--  | many(outer_attribute) vis safety extern crate ident as ident ';'
--h--    {% noSafety $3 (ExternCrate $1 (unspan $2) (unspan $8) (Just (unspan $6)) ($1 # $2 # $4 # $>)) }
--h--  | many(outer_attribute) vis const safety  fn ident generics fn_decl(arg_named) where_clause inner_attrs_block
--h--    { Fn ($1 ++ fst $>) (unspan $2) (unspan $6) $8 (unspan $4) Const Rust ($7 `withWhere` $9) (snd $>) ($1 # $2 # $3 # snd $>) }
--h--  | many(outer_attribute) vis safety extern abi fn ident generics fn_decl(arg_named) where_clause inner_attrs_block
--h--    { Fn ($1 ++ fst $>) (unspan $2) (unspan $7) $9 (unspan $3) NotConst $5 ($8 `withWhere` $10) (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
--h--  | many(outer_attribute) vis safety            fn ident generics fn_decl(arg_named) where_clause inner_attrs_block
--h--    { Fn ($1 ++ fst $>) (unspan $2) (unspan $5) $7 (unspan $3) NotConst Rust ($6 `withWhere` $8) (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
--h--  | many(outer_attribute) vis mod ident ';'
--h--    { Mod $1 (unspan $2) (unspan $4) Nothing ($1 # $2 # $3 # $>) }
--h--  | many(outer_attribute) vis mod ident '{'             many(mod_item) '}'
--h--    { Mod $1 (unspan $2) (unspan $4) (Just $6) ($1 # $2 # $3 # $>) }
--h--  | many(outer_attribute) vis mod ident '{' inner_attrs many(mod_item) '}'
--h--    { Mod ($1 ++ toList $6) (unspan $2) (unspan $4) (Just $7) ($1 # $2 # $3 # $>) }
--h--  | many(outer_attribute) vis safety extern abi '{'             many(foreign_item) '}'
--h--    {% noSafety $3 (ForeignMod $1 (unspan $2) $5 $7 ($1 # $2 # $4 # $>)) }
--h--  | many(outer_attribute) vis safety extern abi '{' inner_attrs many(foreign_item) '}'
--h--    {% noSafety $3 (ForeignMod ($1 ++ toList $7) (unspan $2) $5 $8 ($1 # $2 # $4 # $>)) }
--h--  | many(outer_attribute) vis struct ident generics struct_decl_args
--h--    { StructItem $1 (unspan $2) (unspan $4) (snd $6) ($5 `withWhere` fst $6) ($1 # $2 # $3 # snd $>) }
--h--  | many(outer_attribute) vis union ident generics struct_decl_args
--h--    { Union $1 (unspan $2) (unspan $4) (snd $6) ($5 `withWhere` fst $6) ($1 # $2 # $3 # snd $>) }
--h--  | many(outer_attribute) vis enum ident generics where_clause '{' sep_byT(enum_def,',') '}'
--h--    { Enum $1 (unspan $2) (unspan $4) $8 ($5 `withWhere` $6) ($1 # $2 # $3 # $>) }
--h--  | many(outer_attribute) vis safety trait ident generics ':' sep_by1T(ty_param_bound,'+') where_clause '{' many(trait_item) '}'
--h--    { Trait $1 (unspan $2) (unspan $5) False (unspan $3) ($6 `withWhere` $9) (toList $8) $11 ($1 # $2 # $3 # $4 # $>) }
--h--  | many(outer_attribute) vis safety trait ident generics where_clause '{' many(trait_item) '}'
--h--    { Trait $1 (unspan $2) (unspan $5) False (unspan $3) ($6 `withWhere` $7) [] $9 ($1 # $2 # $3 # $4 # $>) }
--h--  | many(outer_attribute) vis safety auto trait ident generics ':' sep_by1T(ty_param_bound,'+') where_clause '{' many(trait_item) '}'
--h--    { Trait $1 (unspan $2) (unspan $6) True (unspan $3) ($7 `withWhere` $10) (toList $9) $12 ($1 # $2 # $3 # $5 # $>) }
--h--  | many(outer_attribute) vis safety auto trait ident generics where_clause '{' many(trait_item) '}'
--h--    { Trait $1 (unspan $2) (unspan $6) True (unspan $3) ($7 `withWhere` $8) [] $10 ($1 # $2 # $3 # $5 # $>) }
--h--  | many(outer_attribute) vis safety trait ident generics '=' sep_by1T(ty_param_bound,'+') ';'
--h--    {% noSafety $3 (TraitAlias $1 (unspan $2) (unspan $5) $6 (toNonEmpty $8) ($1 # $2 # $3 # $>)) }
--h--  | many(outer_attribute) vis         safety impl generics ty_prim              where_clause '{' impl_items '}'
--h--    { Impl ($1 ++ fst $9) (unspan $2) Final (unspan $3) Positive ($5 `withWhere` $7) Nothing $6 (snd $9) ($1 # $2 # $3 # $4 # $5 # $>) }
--h--  | many(outer_attribute) vis default safety impl generics ty_prim              where_clause '{' impl_items '}'
--h--    { Impl ($1 ++ fst $10) (unspan $2) Default (unspan $4) Positive ($6 `withWhere` $8) Nothing $7 (snd $10) ($1 # $2 # $3 # $4 # $5 # $>) }
--h--  | many(outer_attribute) vis         safety impl generics '(' ty_no_plus ')'   where_clause '{' impl_items '}'
--h--    { Impl ($1 ++ fst $11) (unspan $2) Final (unspan $3) Positive ($5 `withWhere` $9) Nothing (ParenTy $7 ($6 # $8)) (snd $11) ($1 # $2 # $3 # $4 # $5 # $>) }
--h--  | many(outer_attribute) vis default safety impl generics '(' ty_no_plus ')'   where_clause '{' impl_items '}'
--h--    { Impl ($1 ++ fst $12) (unspan $2) Default (unspan $4) Positive ($6 `withWhere` $10) Nothing (ParenTy $8 ($7 # $9)) (snd $12) ($1 # $2 # $3 # $4 # $5 # $>) }
--h--  | many(outer_attribute) vis         safety impl generics '!' trait_ref for ty where_clause '{' impl_items '}'
--h--    { Impl ($1 ++ fst $12) (unspan $2) Final (unspan $3) Negative ($5 `withWhere` $10) (Just $7) $9 (snd $12) ($1 # $2 # $3 # $4 # $5 # $>) }
--h--  | many(outer_attribute) vis default safety impl generics '!' trait_ref for ty where_clause '{' impl_items '}'
--h--    { Impl ($1 ++ fst $13) (unspan $2) Default (unspan $4) Negative ($6 `withWhere` $11) (Just $8) $10 (snd $13) ($1 # $2 # $3 # $4 # $5 # $>) }
--h--  | many(outer_attribute) vis         safety impl generics     trait_ref for ty where_clause '{' impl_items '}'
--h--    { Impl ($1 ++ fst $11) (unspan $2) Final (unspan $3) Positive ($5 `withWhere` $9) (Just $6) $8 (snd $11) ($1 # $2 # $3 # $4 # $5 # $>) }
--h--  | many(outer_attribute) vis default safety impl generics     trait_ref for ty where_clause '{' impl_items '}'
--h--    { Impl ($1 ++ fst $12) (unspan $2) Default (unspan $4) Positive ($6 `withWhere` $10) (Just $7) $9 (snd $12) ($1 # $2 # $3 # $4 # $5 # $>) }
--h--
--h---- Most general type of item
--h--mod_item :: { Item Span }
--h--  : ntItem                                             { $1 }
--h--  | gen_item(vis)                                      { $1 }
--h--  | many(outer_attribute) expr_path '!' ident '[' token_stream ']' ';'
--h--    { macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>) }
--h--  | many(outer_attribute) expr_path '!'       '[' token_stream ']' ';'
--h--    { macroItem $1 Nothing            (Mac $2 $5 ($2 # $>)) ($1 # $2 # $>) }
--h--  | many(outer_attribute) expr_path '!' ident '(' token_stream ')' ';'
--h--    { macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>) }
--h--  | many(outer_attribute) expr_path '!'       '(' token_stream ')' ';'
--h--    { macroItem $1 Nothing            (Mac $2 $5 ($2 # $>)) ($1 # $2 # $>) }
--h--  | many(outer_attribute) expr_path '!' ident '{' token_stream '}'
--h--    { macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>) }
--h--  | many(outer_attribute) expr_path '!'       '{' token_stream '}'
--h--    { macroItem $1 Nothing            (Mac $2 $5 ($2 # $>)) ($1 # $2 # $>) }
--h--
--h--foreign_item :: { ForeignItem Span }
--h--  : many(outer_attribute) vis static     ident ':' ty ';'
--h--    { ForeignStatic $1 (unspan $2) (unspan $4) $6 Immutable ($1 # $2 # $>) }
--h--  | many(outer_attribute) vis static mut ident ':' ty ';'
--h--    { ForeignStatic $1 (unspan $2) (unspan $5) $7 Mutable ($1 # $2 # $>) }
--h--  | many(outer_attribute) vis fn ident generics fn_decl(arg_named) where_clause ';'
--h--    { ForeignFn $1 (unspan $2) (unspan $4) $6 ($5 `withWhere` $7) ($1 # $2 # $>) }
--h--  | many(outer_attribute) vis type ident ';'
--h--    { ForeignTy $1 (unspan $2) (unspan $4) ($1 # $2 # $>) }
--h--
--h---- parse_generics
--h---- Leaves the WhereClause empty
--h--generics :: { Generics Span }
--h--  : ntGenerics                                                      { $1 }
--h--  | '<' sep_by1(lifetime_def,',') ',' sep_by1T(ty_param,',') gt '>' { Generics (toList $2) (toList $4) (WhereClause [] mempty) ($1 # $>) }
--h--  | '<' sep_by1T(lifetime_def,',')                           gt '>' { Generics (toList $2) []          (WhereClause [] mempty) ($1 # $>) }
--h--  | '<'                               sep_by1T(ty_param,',') gt '>' { Generics []          (toList $2) (WhereClause [] mempty) ($1 # $>) }
--h--  | '<'                                                      gt '>' { Generics []          []          (WhereClause [] mempty) ($1 # $>) }
--h--  | {- empty -}                                                     { Generics []          []          (WhereClause [] mempty) mempty }
--h--
--h--ty_param :: { TyParam Span }
--h--  : many(outer_attribute) ident                                              { TyParam $1 (unspan $2) [] Nothing ($1 # $>) }
--h--  | many(outer_attribute) ident ':' sep_by1T(ty_param_bound_mod,'+')         { TyParam $1 (unspan $2) (toList $4) Nothing ($1 # $>) }
--h--  | many(outer_attribute) ident                                      '=' ty  { TyParam $1 (unspan $2) [] (Just $>) ($1 # $>) }
--h--  | many(outer_attribute) ident ':' sep_by1T(ty_param_bound_mod,'+') '=' ty  { TyParam $1 (unspan $2) (toList $4) (Just $>) ($1 # $>) }
--h--
--h--
--h--struct_decl_args :: { (WhereClause Span, VariantData Span) }
--h--  : where_clause ';'                                         { ($1, UnitD ($1 # $>)) }
--h--  | where_clause '{' sep_byT(struct_decl_field,',') '}'      { ($1, StructD $3 ($1 # $>)) }
--h--  | '(' sep_byT(tuple_decl_field,',') ')' where_clause ';'   { ($4, TupleD $2 ($1 # $>)) }
--h--
--h--struct_decl_field :: { StructField Span }
--h--  : many(outer_attribute) vis ident ':' ty                   { StructField (Just (unspan $3)) (unspan $2) $5 $1 ($1 # $2 # $5) }
--h--
--h--tuple_decl_field :: { StructField Span }
--h--  : many(outer_attribute) vis ty                             { StructField Nothing (unspan $2) $3 $1 ($1 # $2 # $3) }
--h--
--h--enum_def :: { Variant Span }
--h--  : many(outer_attribute) ident '{' sep_byT(struct_decl_field,',') '}'  { Variant (unspan $2) $1 (StructD $4 ($3 # $5)) Nothing ($1 # $2 # $>) }
--h--  | many(outer_attribute) ident '(' sep_byT(tuple_decl_field,',')  ')'  { Variant (unspan $2) $1 (TupleD $4 ($3 # $5)) Nothing ($1 # $2 # $>) }
--h--  | many(outer_attribute) ident initializer                             { Variant (unspan $2) $1 (UnitD mempty) $3 ($1 # $2 # $>) }
--h--
--h--
--h---- parse_where_clause
--h--where_clause :: { WhereClause Span }
--h--  : {- empty -}                                        { WhereClause [] mempty }
--h--  | ntWhereClause                                      { $1 }
--h--  | where sep_by(where_predicate,',')      %prec WHERE { WhereClause $2 ($1 # $2) }
--h--  | where sep_by1(where_predicate,',') ',' %prec WHERE { WhereClause (toList $2) ($1 # $3) }
--h--
--h--where_predicate :: { WherePredicate Span }
--h--  : lifetime                                               { RegionPredicate $1 [] (spanOf $1) }
--h--  | lifetime ':' sep_by1T(lifetime,'+')                    { RegionPredicate $1 (toList $3) ($1 # $3) }
--h--  | no_for_ty                                     %prec EQ { BoundPredicate [] $1 [] (spanOf $1) }
--h--  | no_for_ty '='  ty                                      { EqPredicate $1 $3 ($1 # $3) }
--h--  | no_for_ty '==' ty                                      { EqPredicate $1 $3 ($1 # $3) }
--h--  | no_for_ty ':' sep_by1T(ty_param_bound_mod,'+')         { BoundPredicate [] $1 (toList $3) ($1 # $3) }
--h--  | for_lts no_for_ty                                      { BoundPredicate (unspan $1) $2 [] ($1 # $2) }
--h--  | for_lts no_for_ty ':' sep_by1T(ty_param_bound_mod,'+') { BoundPredicate (unspan $1) $2 (toList $4) ($1 # $>) }
--h--
--h--impl_items :: { ([Attribute Span], [ImplItem Span]) }
--h--  :             many(impl_item)  { ([], $1) }
--h--  | inner_attrs many(impl_item)  { (toList $1, $2) }
--h--
--h--impl_item :: { ImplItem Span }
--h--  : ntImplItem                                          { $1 }
--h--  | many(outer_attribute) vis def type ident '=' ty ';'           { TypeI $1 (unspan $2) (unspan $3) (unspan $5) $7 ($1 # $2 # $3 # $4 # $>) }
--h--  | many(outer_attribute) vis def const ident ':' ty '=' expr ';' { ConstI $1 (unspan $2) (unspan $3) (unspan $5) $7 $9 ($1 # $2 # $3 # $4 # $>) }
--h--  | many(outer_attribute)     def mod_mac                         { MacroI $1 (unspan $2) $3 ($1 # $2 # $>) }
--h--  | many(outer_attribute) vis def const safety fn ident generics fn_decl_with_self_named where_clause inner_attrs_block
--h--    { let methodSig = MethodSig (unspan $5) Const Rust $9; generics = $8 `withWhere` $10
--h--      in MethodI ($1 ++ fst $>) (unspan $2) (unspan $3) (unspan $7) generics methodSig (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
--h--  | many(outer_attribute) vis def safety ext_abi fn ident generics fn_decl_with_self_named where_clause inner_attrs_block
--h--    { let methodSig = MethodSig (unspan $4) NotConst (unspan $5) $9; generics = $8 `withWhere` $10
--h--      in MethodI ($1 ++ fst $>) (unspan $2) (unspan $3) (unspan $7) generics methodSig (snd $>) ($1 # $2 # $3 # $4 # $5 # $6 # snd $>) }
--h--
--h--trait_item :: { TraitItem Span }
--h--  : ntTraitItem                                              { $1 }
--h--  | many(outer_attribute) const ident ':' ty initializer ';' { ConstT $1 (unspan $3) $5 $6 ($1 # $2 # $>) }
--h--  | many(outer_attribute) mod_mac                            { MacroT $1 $2 ($1 # $>) }
--h--  | many(outer_attribute) type ident ';'                     { TypeT $1 (unspan $3) [] Nothing ($1 # $2 # $>) }
--h--  | many(outer_attribute) type ident '=' ty ';'              { TypeT $1 (unspan $3) [] (Just $5) ($1 # $2 # $>) }
--h--  | many(outer_attribute) type ident ':' sep_by1T(ty_param_bound_mod,'+') ';'
--h--    { TypeT $1 (unspan $3) (toList $5) Nothing ($1 # $2 # $>) }
--h--  | many(outer_attribute) type ident ':' sep_by1T(ty_param_bound_mod,'+') '=' ty ';'
--h--    { TypeT $1 (unspan $3) (toList $5) (Just $7) ($1 # $2 # $>) }
--h--  | many(outer_attribute) safety ext_abi fn ident generics fn_decl_with_self_general where_clause ';'
--h--    { let methodSig = MethodSig (unspan $2) NotConst (unspan $3) $7; generics = $6 `withWhere` $8
--h--      in MethodT $1 (unspan $5) generics methodSig Nothing ($1 # $2 # $3 # $4 # $>) }
--h--  | many(outer_attribute) safety ext_abi fn ident generics fn_decl_with_self_general where_clause inner_attrs_block
--h--    { let methodSig = MethodSig (unspan $2) NotConst (unspan $3) $7; generics = $6 `withWhere` $8
--h--      in MethodT ($1 ++ fst $>) (unspan $5) generics methodSig (Just (snd $>)) ($1 # $2 # $3 # $4 # snd $>) }
--h--
--h--safety :: { Spanned Unsafety }
--h--  : {- empty -}             { pure Normal }
--h--  | unsafe                  { Spanned Unsafe (spanOf $1) }
--h--
--h--ext_abi :: { Spanned Abi }
--h--  : {- empty -}             { pure Rust }
--h--  | extern abi              { Spanned $2 (spanOf $1) }
--h--
--h--vis :: { Spanned (Visibility Span) }
--h--  : {- empty -}   %prec VIS { Spanned InheritedV mempty }
--h--  | pub           %prec VIS { Spanned PublicV (spanOf $1) }
--h--  | pub '(' crate ')'       { Spanned CrateV ($1 # $4) }
--h--  | crate                   { Spanned CrateV (spanOf $1) }
--h--  | pub '(' in mod_path ')' { Spanned (RestrictedV $4) ($1 # $5) }
--h--  | pub '(' super ')'       { Spanned (RestrictedV (Path False [PathSegment "super" Nothing (spanOf
--h--  $3)] (spanOf $3))) ($1 # $4) }
--h--  | pub '(' self ')'        { Spanned (RestrictedV (Path False [PathSegment "self" Nothing (spanOf
--h--  $3)] (spanOf $3))) ($1 # $4) }
--h--
--h--def :: { Spanned Defaultness }
--h--  : {- empty -}  %prec DEF        { pure Final }
--h--  | default              { Spanned Default (spanOf $1) }
--h--
--h--use_tree :: { UseTree Span }
--h--  : mod_path                                    { UseTreeSimple $1 Nothing (spanOf $1) }
--h--  | mod_path as ident                           { UseTreeSimple $1 (Just (unspan $3)) ($1 # $3) }
--h--  | mod_path '::' '*'                           { UseTreeGlob $1 ($1 # $3) }
--h--  |          '::' '*'                           { UseTreeGlob (Path True [] (spanOf $1)) ($1 # $2) }
--h--  |               '*'                           { UseTreeGlob (Path False [] mempty) (spanOf $1) }
--h--  | mod_path '::' '{' sep_byT(use_tree,',') '}' { UseTreeNested $1 $4 ($1 # $>) }
--h--  |          '::' '{' sep_byT(use_tree,',') '}' { UseTreeNested (Path True [] (spanOf $1)) $3 ($1 # $>) }
--h--  |               '{' sep_byT(use_tree,',') '}' { UseTreeNested (Path False [] mempty) $2 ($1 # $>) }
--h--
--h---------------------
--h---- Macro related --
--h---------------------
--h--
--h--expr_mac :: { Mac Span }
--h--  : expr_path '!' '[' token_stream ']'     { Mac $1 $4 ($1 # $>) }
--h--  | expr_path '!' '(' token_stream ')'     { Mac $1 $4 ($1 # $>) }
--h--
--h--ty_mac :: { Mac Span }
--h--  : ty_path '!' '[' token_stream ']'       { Mac $1 $4 ($1 # $>) }
--h--  | ty_path '!' '{' token_stream '}'       { Mac $1 $4 ($1 # $>) }
--h--  | ty_path '!' '(' token_stream ')'       { Mac $1 $4 ($1 # $>) }
--h--
--h--mod_mac :: { Mac Span }
--h--  : mod_path '!' '[' token_stream ']' ';'  { Mac $1 $4 ($1 # $>) }
--h--  | mod_path '!' '{' token_stream '}'      { Mac $1 $4 ($1 # $>) }
--h--  | mod_path '!' '(' token_stream ')' ';'  { Mac $1 $4 ($1 # $>) }

token_stream :: { TokenStream }
  : {- empty -}                                { Stream [] }
  | some(token_tree)                           {
      case $1 of
        [tt] -> Tree tt
        tts -> Stream [ Tree tt | tt <- toList tts ]
    }

token_tree :: { TokenTree }
  : ntTT                                       { $1 }
  -- # Delimited
  | '(' token_stream ')'                       { Delimited ($1 # $3) Paren $2 }
  | '{' token_stream '}'                       { Delimited ($1 # $3) Brace $2 }
  | '[' token_stream ']'                       { Delimited ($1 # $3) Bracket $2 }
  -- # Token
  | token                                      { let Spanned t s = $1 in Token s t }

token :: { Spanned Token }
  : '='        { $1 }
  | '<'        { $1 }
  | '>'        { $1 }
  | '!'        { $1 }
  | '~'        { $1 }
  | '-'        { $1 }
  | '/'        { $1 }
  | '+'        { $1 }
  | '*'        { $1 }
  | '%'        { $1 }
  | '^'        { $1 }
  | '&'        { $1 }
  | '|'        { $1 }
  | '<<='      { $1 }
  | '>>='      { $1 }
  | '-='       { $1 }
  | '&='       { $1 }
  | '|='       { $1 }
  | '+='       { $1 }
  | '*='       { $1 }
  | '/='       { $1 }
  | '^='       { $1 }
  | '%='       { $1 }
  | '||'       { $1 }
  | '&&'       { $1 }
  | '=='       { $1 }
  | '!='       { $1 }
  | '<='       { $1 }
  | '>='       { $1 }
  | '<<'       { $1 }
  | '>>'       { $1 }
  -- Structural symbols.
  | '@'        { $1 }
  | '...'      { $1 }
  | '..='      { $1 }
  | '..'       { $1 }
  | '.'        { $1 }
  | ','        { $1 }
  | ';'        { $1 }
  | '::'       { $1 }
  | ':'        { $1 }
  | '->'       { $1 }
  | '<-'       { $1 }
  | '=>'       { $1 }
  | '#'        { $1 }
  | '$'        { $1 }
  | '?'        { $1 }
  | '#!'       { $1 }
  -- Literals.
  | byte       { $1 }
  | char       { $1 }
  | int        { $1 }
  | float      { $1 }
  | str        { $1 }
  | byteStr    { $1 }
  | rawStr     { $1 }
  | rawByteStr { $1 }
  -- Strict keywords used in the language
  | as         { $1 }
  | box        { $1 }
  | break      { $1 }
  | const      { $1 }
  | continue   { $1 }
  | crate      { $1 }
  | else       { $1 }
  | enum       { $1 }
  | extern     { $1 }
  | false      { $1 }
  | fn         { $1 }
  | for        { $1 }
  | if         { $1 }
  | impl       { $1 }
  | in         { $1 }
  | let        { $1 }
  | loop       { $1 }
  | match      { $1 }
  | mod        { $1 }
  | move       { $1 }
  | mut        { $1 }
  | pub        { $1 }
  | ref        { $1 }
  | return     { $1 }
  | Self       { $1 }
  | self       { $1 }
  | static     { $1 }
  | struct     { $1 }
  | super      { $1 }
  | trait      { $1 }
  | true       { $1 }
  | type       { $1 }
  | unsafe     { $1 }
  | use        { $1 }
  | where      { $1 }
  | while      { $1 }
  -- Keywords reserved for future use
  | abstract   { $1 }
  | alignof    { $1 }
  | become     { $1 }
  | do         { $1 }
  | final      { $1 }
  | macro      { $1 }
  | offsetof   { $1 }
  | override   { $1 }
  | priv       { $1 }
  | proc       { $1 }
  | pure       { $1 }
  | sizeof     { $1 }
  | typeof     { $1 }
  | unsized    { $1 }
  | virtual    { $1 }
  -- Weak keywords, have special meaning only in specific contexts.
  | default    { $1 }
  | union      { $1 }
  | catch      { $1 }
  | auto       { $1 }
  | yield      { $1 }
  | dyn        { $1 }
  -- Comments
  | outerDoc   { $1 }
  | innerDoc   { $1 }
  -- Identifiers.
  | IDENT      { $1 }
  | '_'        { $1 }
  -- Lifetimes.
  | LIFETIME   { $1 }

--h--
--h-----------------------
--h---- Just for export --
--h-----------------------
--h--
-- These rules aren't used anywhere in the grammar above, they just provide a more general parsers.

-- Any attribute
export_attribute :: { Attribute Span }
  : inner_attribute { $1 }
  | outer_attribute { $1 }
--h--
--h---- Complete blocks
--h--export_block :: { Block Span }
--h--  : ntBlock                                                { $1 }
--h--  | safety '{' '}'                                         { Block [] (unspan $1) ($1 # $2 # $>) }
--h--  | safety '{' stmts_possibly_no_semi '}'                  { Block [ s | Just s <- $3 ] (unspan $1) ($1 # $2 # $>) }
--h--
{
--h---- | Parser for literals.
--h--parseLit :: P (Lit Span)
--h--
--h---- | Parser for attributes.
--h--parseAttr :: P (Attribute Span)

-- | Parser for types.
parseTy :: P (Ty Span)

--h---- | Parser for patterns.
--h--parsePat :: P (Pat Span)
--h--
--h---- | Parser for statements.
--h--parseStmt :: P (Stmt Span)
--h--
--h---- | Parser for expressions.
--h--parseExpr :: P (Expr Span)
--h--
--h---- | Parser for items.
--h--parseItem :: P (Item Span)
--h--
--h---- | Parser for blocks.
--h--parseBlock :: P (Block Span)
--h--
--h---- | Parser for @impl@ items.
--h--parseImplItem :: P (ImplItem Span)
--h--
--h---- | Parser for @trait@ items.
--h--parseTraitItem :: P (TraitItem Span)
--h--
--h---- | Parser for token trees.
--h--parseTt :: P TokenTree
--h--
--h---- | Parser for token streams.
--h--parseTokenStream :: P TokenStream
--h--
--h---- | Parser for lifetime definitions.
--h--parseLifetimeDef :: P (LifetimeDef Span)
--h--
--h---- | Parser for a type parameter.
--h--parseTyParam :: P (TyParam Span)
--h--
--h---- | Parser for a where clause.
--h--parseWhereClause :: P (WhereClause Span)
--h--
--h---- | Parser for generics (although 'WhereClause' is always empty here).
--h--parseGenerics :: P (Generics Span)

-- | Generate a nice looking error message based on expected tokens
expParseError :: (Spanned Token, [String]) -> P a
expParseError (Spanned t _, exps) = fail $ "Syntax error: unexpected `" ++ show t ++ "'" ++
    case go (sort exps) [] replacements of
      []       -> ""
      [s]      -> " (expected " ++ s ++ ")"
      [s2,s1]  -> " (expected " ++ s1 ++ " or " ++ s2 ++ ")"
      (s : ss) -> " (expected " ++ (reverse ss >>= (++ ", ")) ++ "or " ++ s ++ ")"
  where

  go []     msgs _ = msgs
  go (e:es) msgs rs | e `elem` ignore = go es msgs rs
  go (e:es) msgs [] = go es (e : msgs) []
  go es     msgs ((rep,msg):rs)
    | rep `isSubsequenceOf` es = go (es \\ rep) (msg : msgs) rs
    | otherwise = go es msgs rs

  ignore = words "ntItem ntBlock ntStmt ntPat ntExpr ntTy ntIdent ntPath ntTT" ++
           words "ntArm ntImplItem ntTraitItem ntGenerics ntWhereClause ntArg ntLit"

  replacements = map (\(ks,v) -> (sort ks,v)) $
    [ (expr,                              "an expression"   )

    , (lit,                               "a literal"       )
    , (boolLit,                           "a boolean"       )
    , (byteLit,                           "a byte"          )
    , (charLit,                           "a character"     )
    , (intLit,                            "an int"          )
    , (floatLit,                          "a float"         )
    , (strLit,                            "a string"        )
    , (byteStrLit,                        "a byte string"   )
    , (rawStrLit,                         "a raw string"    )
    , (rawByteStrLit,                     "a raw bytestring")

    , (doc,                               "a doc"           )
    , (outerDoc,                          "an outer doc"    )
    , (innerDoc,                          "an inner doc"    )

    , (identifier,                        "an identifier"   )
    , (lifetime,                          "a lifetime"      )
    ]

  expr :: [String]
  expr = lit ++ identifier ++ lifetime ++
         words "'<' '!' '-' '*' '&' '|' '...' '..=' '..' '::'" ++
         words "'||' '&&' '<<' '(' '[' '{' box break continue" ++
         words "for if loop match move return Self self      " ++
         words "static super unsafe while do default union   " ++
         words "catch auto yield dyn"

  lit = boolLit ++ byteLit ++ charLit ++ intLit ++ floatLit ++ strLit ++
        byteStrLit ++ rawStrLit ++ rawByteStrLit
  boolLit       = words "true false"
  byteLit       = words "byte"
  charLit       = words "char"
  intLit        = words "int"
  floatLit      = words "float"
  strLit        = words "str"
  byteStrLit    = words "byteStr"
  rawStrLit     = words "rawStr"
  rawByteStrLit = words "rawByteStr"

  doc = outerDoc ++ innerDoc
  outerDoc = words "outerDoc"
  innerDoc = words "innerDoc"

  identifier = words "IDENT"
  lifetime = words "LIFETIME"

-- | Convert an 'IdentTok' into an 'Ident'
toIdent :: Spanned Token -> Spanned Ident
toIdent (Spanned (IdentTok i) s) = Spanned i s

--h---- | Try to convert an expression to a statement given information about whether there is a trailing
--h---- semicolon
--h--toStmt :: Expr Span -> Bool -> Bool -> Span -> Stmt Span
--h--toStmt (MacExpr a m s) hasSemi isBlock | hasSemi = MacStmt m SemicolonMac a
--h--                                       | isBlock = MacStmt m BracesMac a
--h--toStmt e hasSemi _ = (if hasSemi then Semi else NoSemi) e
--h--
--h---- | Return the second argument, as long as the visibility is 'InheritedV'
--h--noVis :: Spanned (Visibility Span) -> a -> P a
--h--noVis (Spanned InheritedV _) x = pure x
--h--noVis _ _ = fail "visibility is not allowed here"
--h--
--h---- | Fill in the where clause in a generic
--h--withWhere :: Generics a -> WhereClause a -> Generics a
--h--withWhere (Generics l t _ x) w = Generics l t w x
--h--
--h---- | Return the second argument, as long as the safety is 'Normal'
--h--noSafety :: Spanned Unsafety -> a -> P a
--h--noSafety (Spanned Normal _) x = pure x
--h--noSafety _ _ = fail "safety is not allowed here"
--h--
--h---- | Make a macro item, which may be a 'MacroDef'
--h--macroItem :: [Attribute Span] -> (Maybe Ident) -> Mac Span -> Span -> Item Span
--h--macroItem as (Just i) (Mac (Path False [PathSegment "macro_rules" Nothing _] _) tts _) x = MacroDef as i tts x
--h--macroItem as i mac x = MacItem as i mac x
--h--
--h---- | Add attributes to an expression
--h--addAttrs :: [Attribute Span] -> Expr Span -> Expr Span
--h--addAttrs as (Box as' e s)            = Box (as ++ as') e s
--h--addAttrs as (InPlace as' e1 e2 s)    = InPlace (as ++ as') e1 e2 s
--h--addAttrs as (Vec as' e s)            = Vec (as ++ as') e s
--h--addAttrs as (Call as' f es s)        = Call (as ++ as') f es s
--h--addAttrs as (MethodCall as' i s tys es s') = MethodCall (as ++ as') i s tys es s'
--h--addAttrs as (TupExpr as' e s)        = TupExpr (as ++ as') e s
--h--addAttrs as (Binary as' b e1 e2 s)   = Binary (as ++ as') b e1 e2 s
--h--addAttrs as (Unary as' u e s)        = Unary (as ++ as') u e s
--h--addAttrs as (Lit as' l s)            = Lit (as ++ as') l s
--h--addAttrs as (Cast as' e t s)         = Cast (as ++ as') e t s
--h--addAttrs as (TypeAscription as' e t s) = TypeAscription (as ++ as') e t s
--h--addAttrs as (If as' e1 b b2 s)       = If (as ++ as') e1 b b2 s
--h--addAttrs as (IfLet as' p e b em s)   = IfLet (as ++ as') p e b em s
--h--addAttrs as (While as' e b l s)      = While (as ++ as') e b l s
--h--addAttrs as (WhileLet as' p e b l s) = WhileLet (as ++ as') p e b l s
--h--addAttrs as (ForLoop as' p e b l s)  = ForLoop (as ++ as') p e b l s
--h--addAttrs as (Loop as' b l s)         = Loop (as ++ as') b l s
--h--addAttrs as (Match as' e a s)        = Match (as ++ as') e a s
--h--addAttrs as (Closure as' m c f e s)  = Closure (as ++ as') m c f e s
--h--addAttrs as (BlockExpr as' b s)      = BlockExpr (as ++ as') b s
--h--addAttrs as (Catch as' b s)          = Catch (as ++ as') b s
--h--addAttrs as (Assign as' e1 e2 s)     = Assign (as ++ as') e1 e2 s
--h--addAttrs as (AssignOp as' b e1 e2 s) = AssignOp (as ++ as') b e1 e2 s
--h--addAttrs as (FieldAccess as' e i s)  = FieldAccess (as ++ as') e i s
--h--addAttrs as (TupField as' e i s)     = TupField (as ++ as') e i s
--h--addAttrs as (Index as' e1 e2 s)      = Index (as ++ as') e1 e2 s
--h--addAttrs as (Range as' e1 e2 r s)    = Range (as ++ as') e1 e2 r s
--h--addAttrs as (PathExpr as' q p s)     = PathExpr (as ++ as') q p s
--h--addAttrs as (AddrOf as' m e s)       = AddrOf (as ++ as') m e s
--h--addAttrs as (Break as' l e s)        = Break (as ++ as') l e s
--h--addAttrs as (Continue as' l s)       = Continue (as ++ as') l s
--h--addAttrs as (Ret as' e s)            = Ret (as ++ as') e s
--h--addAttrs as (MacExpr as' m s)        = MacExpr (as ++ as') m s
--h--addAttrs as (Struct as' p f e a)     = Struct (as ++ as') p f e a
--h--addAttrs as (Repeat as' e1 e2 s)     = Repeat (as ++ as') e1 e2 s
--h--addAttrs as (ParenExpr as' e s)      = ParenExpr (as ++ as') e s
--h--addAttrs as (Try as' e s)            = Try (as ++ as') e s
--h--addAttrs as (Yield as' e s)          = Yield (as ++ as') e s
--h--
--h--
--h---- | Given a 'LitTok' token that is expected to result in a valid literal, construct the associated
--h---- literal. Note that this should _never_ fail on a token produced by the lexer.
--h--lit :: Spanned Token -> Lit Span
--h--lit (Spanned (IdentTok (Ident "true" False _)) s) = Bool True Unsuffixed s
--h--lit (Spanned (IdentTok (Ident "false" False _)) s) = Bool False Unsuffixed s
--h--lit (Spanned (LiteralTok litTok suffix_m) s) = translateLit litTok suffix s
--h--  where
--h--    suffix = case suffix_m of
--h--               Nothing -> Unsuffixed
--h--               (Just "isize") -> Is
--h--               (Just "usize") -> Us
--h--               (Just "i8")    -> I8
--h--               (Just "u8")    -> U8
--h--               (Just "i16")   -> I16
--h--               (Just "u16")   -> U16
--h--               (Just "i32")   -> I32
--h--               (Just "u32")   -> U32
--h--               (Just "i64")   -> I64
--h--               (Just "u64")   -> U64
--h--               (Just "i128")  -> I128
--h--               (Just "u128")  -> U128
--h--               (Just "f32")   -> F32
--h--               (Just "f64")   -> F64
--h--               _ -> error "invalid literal"
--h--
--h--isTraitTyParamBound TraitTyParamBound{} = True
--h--isTraitTyParamBound _ = False
--h--
--h---- | Parse a source file
--h--parseSourceFile :: P (SourceFile Span)
--h--parseSourceFile = do
--h--  sh <- lexShebangLine
--h--  (as,items) <- parseSourceFileContents
--h--  pure (SourceFile sh as items)
--h--
--h---- | Nudge the span endpoints of a 'Span' value
--h--nudge :: Int -> Int -> Span -> Span
--h--nudge leftSide rightSide (Span l r) = Span l' r'
--h--  where l' = incPos l leftSide
--h--        r' = incPos r rightSide
--h--
--h--
--h---- Functions related to `NonEmpty` that really should already exist...
--h--
--h---- | Append an element to a list to get a nonempty list (flipped version of '(:|)')
--h--(|:) :: [a] -> a -> NonEmpty a
--h--[] |: y = y :| []
--h--(x:xs) |: y = x :| (xs ++ [y])
--h--
--h---- | Append an element to a nonempty list to get anothg nonempty list (flipped version of '(<|)')
--h--(|>) :: NonEmpty a -> a -> NonEmpty a
--h--(x:|xs) |> y = x :| (xs ++ [y])
--h--
--h--}
