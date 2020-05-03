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
%name parseLit lit
%name parseAttr export_attribute

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
--h----additional inout out from fortran
--h--  out            { Spanned (IdentTok "out") _ }
--h--  inout          { Spanned (IdentTok "inout") _ }
  format         { Spanned (IdentTok "format") _ }
  kind         { Spanned (IdentTok "kind") _ }

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

  ntTy           { Spanned (Interpolated (NtTy $$)) _ }
  ntIdent        { Spanned (Interpolated (NtIdent _)) _ }
  ntPath         { Spanned (Interpolated (NtPath $$)) _ }
  ntTT           { Spanned (Interpolated (NtTT $$)) _ }
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

gt :: { () }
  : {- empty -}   {%% \(Spanned tok s) ->
      let s' = nudge 1 0 s; s'' = nudge 0 (-1) s in
      case tok of
        GreaterGreater      -> pushToken (Spanned Greater s')      *> pushToken (Spanned Greater s'')
        GreaterEqual        -> pushToken (Spanned Equal s')        *> pushToken (Spanned Greater s'')
        GreaterGreaterEqual -> pushToken (Spanned GreaterEqual s') *> pushToken (Spanned Greater s'')
        _                   -> pushToken (Spanned tok s)
    }


-------------
-- Utility --
-------------

-- | One or more occurences of 'p'
some(p) :: { Reversed NonEmpty p }
  : some(p) p             { let Reversed xs = $1 in Reversed ($2 <| xs) }
  | p                     { [$1] }

-- | One or more occurences of 'p', seperated by 'sep'
sep_by1(p,sep) :: { Reversed NonEmpty p }
  : sep_by1(p,sep) sep p  { let Reversed xs = $1 in Reversed ($3 <| xs) }
  | p                     { [$1] }


-- | One or more occurrences of 'p', seperated by 'sep', optionally ending in 'sep'
sep_by1T(p,sep) :: { Reversed NonEmpty p }
  : sep_by1(p,sep) sep    { $1 }
  | sep_by1(p,sep)        { $1 }

-- | Zero or more occurences of 'p', seperated by 'sep', optionally ending in 'sep' (only if there
-- is at least one 'p')
sep_byT(p,sep) :: { [ p ] }
  : sep_by1T(p,sep)       { toList $1 }
  | {- empty -}           { [] }

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


lit :: { Lit Span }
  : ntLit             { $1 }
  | byte              { lit $1 }
  | char              { lit $1 }
  | int               { lit $1 }
  | float             { lit $1 }
  | true              { lit $1 }
  | false             { lit $1 }
  | kind              { lit $1 }
  | string            { $1 }

string :: { Lit Span }
  : str               { lit $1 }
  | rawStr            { lit $1 }
  | byteStr           { lit $1 }
  | rawByteStr        { lit $1 }

qual_path(segs) :: { Spanned (QSelf Span, Path Span) }
  : '<' qual_path_suf(segs)                    { let Spanned x _ = $2 in Spanned x ($1 # $2) }
qual_path_suf(segs) :: { Spanned (QSelf Span, Path Span) }
  : ty '>' '::' segs                { Spanned (QSelf $1 0, Path False (toList $4) (spanOf $4)) ($1 # $>) }

-- parse_generic_args() but with the '<' '>'
generic_values :: { Spanned ([Lifetime Span], [Ty Span], [(Ident, Ty Span)]) }
  : '<' sep_by1(lifetime,',')  ',' sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>'
    { Spanned (toList $2,      toList $4, toList $6) ($1 # $>) }
binding :: { (Ident, Ty Span) }
  : ident '=' ty                             { (unspan $1, $3) }

-- Type related:
-- parse_path(PathStyle::Type)
ty_path :: { Path Span }
  : ntPath                                   { $1 }
  | path_segments_without_colons             { Path False $1 (spanOf $1) }
  | '::' path_segments_without_colons        { Path True $2 ($1 # $2) }

ty_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(path_segments_without_colons)  { $1 }

-- parse_path_segments_without_colons()
path_segments_without_colons :: { [PathSegment Span] }
  : sep_by1(path_segment_without_colons, '::') %prec SEG  { toList $1 }

-- No corresponding function - see path_segments_without_colons
path_segment_without_colons :: { PathSegment Span }
  : self_or_ident path_parameter                     { PathSegment (unspan $1) $2 ($1 # $>) }

path_parameter  :: { Maybe (PathParameters Span) }
  : generic_values                           { let (lts, tys, bds) = unspan $1
                                               in Just (AngleBracketed lts tys bds (spanOf $1)) }
  | '(' sep_byT(ty,',') ')'                  { Just (Parenthesized $2 Nothing ($1 # $>)) }
  | '(' sep_byT(ty,',') ')' '->' ty_no_plus  { Just (Parenthesized $2 (Just $>) ($1 # $>)) }
  | {- empty -}                  %prec IDENT { Nothing }


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

lifetime :: { Lifetime Span }
  : LIFETIME                         { let Spanned (LifetimeTok (Ident l _ _)) s = $1 in Lifetime l s }

no_for_ty_prim :: { Ty Span }
  : ty_path               %prec PATH { PathTy Nothing $1 ($1 # $>) }

-- parse_ty()
-- See https://github.com/rust-lang/rfcs/blob/master/text/0438-precedence-of-plus.md
-- All types, including trait types with plus
ty :: { Ty Span }
  : ty ':' expr        { Array $1 $3 ($1 # $>) }
  | self_or_ident      { FType (unspan $1) (TupExpr [] [] ($1 # $>)) ($1 # $>) }
  | self_or_ident expr { FType (unspan $1) $2 ($1 # $>) }
  | '(' ')'            { TupTy [] ($1 # $>) }
  | '*' ty             { Ptr Mutable $2 ($1 # $2) }
--  | ty_no_plus                                                    { $1 }
--  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+') { TraitObject ($1 <| toNonEmpty $3) ($1 # $3) }

-- parse_ty_no_plus()
ty_no_plus :: { Ty Span }
  : ntTy                             { $1 }
  | no_for_ty                        { $1 }
--  | for_ty_no_plus                   { $1 }

-- All (non-sum) types not starting with a 'for'
no_for_ty :: { Ty Span }
  : '(' ')'                          { TupTy [] ($1 # $>) }
  | '(' ty ')'                       { ParenTy $2 ($1 # $3) }
  | '(' ty ',' ')'                   { TupTy [$2] ($1 # $4) }
  | '(' ty ',' sep_by1T(ty,',') ')'  { TupTy ($2 : toList $4) ($1 # $5) }
  | ty_qual_path                     { PathTy (Just (fst (unspan $1))) (snd (unspan $1)) (spanOf $1) }
  | no_for_ty_prim                   { $1 }

expr :: { Expr Span }
  : lit                                   { Lit [] $1 (spanOf $1) }
  | expr '=' expr  { Assign [] $1 $3 ($1 # $>) }
  | '(' expr ')'   { TupExpr [] [$2] ($1 # $>) }
  | '(' expr ',' sep_by1T(expr,',') ')'   { TupExpr [] ($2:toList $4) ($1 # $>) }

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

-- These rules aren't used anywhere in the grammar above, they just provide a more general parsers.

-- Any attribute
export_attribute :: { Attribute Span }
  : inner_attribute { $1 }
  | outer_attribute { $1 }
{

-- | Parser for types.
parseTy :: P (Ty Span)


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

-- | Given a 'LitTok' token that is expected to result in a valid literal, construct the associated
-- literal. Note that this should _never_ fail on a token produced by the lexer.
lit :: Spanned Token -> Lit Span
lit (Spanned (IdentTok (Ident i _ _)) s) = translateLit (StrRawTok i $ length i) Unsuffixed s
lit (Spanned (IdentTok (Ident "true" False _)) s) = Bool True Unsuffixed s
lit (Spanned (IdentTok (Ident "false" False _)) s) = Bool False Unsuffixed s
lit (Spanned (LiteralTok litTok suffix_m) s) = translateLit litTok Unsuffixed s
  where
    suffix = case suffix_m of
               Nothing -> Unsuffixed
               (Just "isize") -> Is
               (Just "usize") -> Us
               (Just "i8")    -> I8
               (Just "u8")    -> U8
               (Just "i16")   -> I16
               (Just "u16")   -> U16
               (Just "i32")   -> I32
               (Just "u32")   -> U32
               (Just "i64")   -> I64
               (Just "u64")   -> U64
               (Just "i128")  -> I128
               (Just "u128")  -> U128
               (Just "f32")   -> F32
               (Just "f64")   -> F64
               _ -> error "invalid literal"

-- | Nudge the span endpoints of a 'Span' value
nudge :: Int -> Int -> Span -> Span
nudge leftSide rightSide (Span l r) = Span l' r'
  where l' = incPos l leftSide
        r' = incPos r rightSide
}
