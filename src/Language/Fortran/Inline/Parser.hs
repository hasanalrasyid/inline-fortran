{-|
Module      : Language.Fortran.Inline.Parser
Description : Parser for Quasiquotes
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Fortran.Inline.Parser where

import Language.Fortran.Inline.Pretty ( renderType )

import Language.Rust.Syntax        ( Token(..), Delim(..), Ty(..))

--import qualified Language.Fortran.Lexer.FreeForm as Free ( collectFreeTokens, Token(..), lexer', initParseState, LexAction(..),AlexInput(..),AlexReturn(..),resetLexeme,scActual,normaliseStartCode,User(..),alexScanUser,StartCode(..),StartCodeStatus(..))
import qualified Language.Fortran.Lexer.FreeForm as Free
import qualified Language.Fortran.Inline.Lexer as L
import qualified Data.ByteString.Char8 as B8
import qualified Language.Fortran.ParserMonad as FPM
import qualified Language.Fortran.Util.Position as FP
import Control.Monad.State (get)

import Language.Rust.Parser
import Language.Rust.Data.Position ( Spanned(..), Position(..) )
import Language.Rust.Data.Ident    ( Ident(..) )

import Language.Haskell.TH         ( Q, runIO )

import Control.Monad               ( void )

import Language.Fortran.Util.ModFile ( emptyModFiles )
import Language.Fortran.Input ( parseSrcString )

-- All the tokens we deal with are 'Spanned'...
type SpTok = Spanned Token

-- | Result of parsing a quasiquote. Quasiquotes are of the form
-- @<ty> { <block> }@ where the @<block>@ possibly contains escaped arguments
-- in the form of @$(<ident>: <ty>)@.
data FortQuasiquoteParse = FQParse

  -- | leading type (corresponding to the return type of the quasiquote)
  { tyF :: L.Token

  -- | body tokens, with @$(<ident>: <ty>)@ escapes replaced by just @ident@
  , bodyF :: [L.Token]

  -- | escaped arguments
  , variablesF :: [(String, L.Token)]

  } deriving (Show)
data RustQuasiquoteParse = QQParse

  -- | leading type (corresponding to the return type of the quasiquote)
  { ty :: Ty Span

  -- | body tokens, with @$(<ident>: <ty>)@ escapes replaced by just @ident@
  , body :: [SpTok]

  -- | escaped arguments
  , variables :: [(String, Ty Span)]

  } deriving (Show)


-- | Parse an inline Rust quasiquote. The grammar for a quasiquote is
--
-- @
--     <quasiquote> ::= <ty> '{' <body> '}'
--
--     <body>       ::= '$' '(' <ident> ':' <ty> ')' <body>
--                    | <tok> <body>
--                    | {- empty -}
-- @

clearBracket :: String -> String
clearBracket s = init $ tail $ reverse $ dropWhile (/= '}') $ reverse $ dropWhile (/='{') s

--execParserFotran :: String -> Either ParseFail [SpTok]
execParserFortran s =
  let a = L.collectFreeTokens FPM.Fortran95 $ B8.pack $ clearBracket s
  in if | a == [] -> Left (ParseFail (Position 0 0 0) "error execParserFortran")
        | otherwise -> Right a

isLBrace :: L.Token -> Bool
isLBrace (L.TLBrace _) = True
isLBrace _ = False

isRBrace :: L.Token -> Bool
isRBrace (L.TRBrace _) = True
isRBrace _ = False
-- Parse the body of the quasiquote
--parseBodyF :: [SpTok] -> [(String, Ty Span)] -> [SpTok]
--          -> Q ([SpTok], [(String, Ty Span)])
parseBodyF :: [L.Token] -> [(String, L.Token)] -> [L.Token]
           -> Q ([L.Token],[(String, L.Token)])
parseBodyF toks vars rest1 =
  case rest1 of
    [] -> pure (reverse toks, vars)

    ( L.TSigil l            :
      _  :
      L.TId _ i       :
      _ : rest2 ) -> do
       let i' = i
       let newT = L.TId l i
       parseBodyF ( newT : toks) ((i',newT):vars) rest2

    (tok : rest2) -> parseBodyF (tok : toks) vars rest2
  where
    -- Add it to 'vars' if it isn't a duplicate
    dupMsg ia t1a t2a = concat $ "FVariable `": show ia : ": ": show t1a
                               : "' has already been given type `"
                               : show t2a: "'": []

clearRBrace l = let (_:r) = dropWhile (not . isRBrace) $ reverse l
                 in reverse r

parseFQ :: String -> Q FortQuasiquoteParse
parseFQ input = do
  let lexer = lexTokens lexNonSpace
  let stream = inputStreamFromString input
  -- Lex the quasiquote tokens

  rest1 <-  case execParser lexer stream initPos of
      Left (ParseFail _ msg) -> fail msg
      Right parsed -> pure parsed

  r1 <- case execParserFortran input of
    Left (ParseFail _ msg) -> fail msg
    Right parsed -> pure parsed
  (leadTy , r2) <-
    case break isLBrace r1 of
      (_, []) -> fail "Ran out of input parsing leading type in quasiquote Fortran"
      (tyToks, lBrace : rest2) -> pure (head tyToks, clearRBrace rest2)

  -- Split off the leading type's tokens

  -- Parse leading type

  -- Parse body of quasiquote
  (bodyTF, varsF) <- parseBodyF [] [] r2
  let fq = (FQParse leadTy bodyTF varsF)

  -- Done!
  return fq

parseQQ :: String -> Q RustQuasiquoteParse
parseQQ input = do
  let lexer = lexTokens lexNonSpace
  let stream = inputStreamFromString input
  -- Lex the quasiquote tokens

  rest1 <-  case execParser lexer stream initPos of
      Left (ParseFail _ msg) -> fail msg
      Right parsed -> pure parsed

  r1 <- case execParserFortran input of
    Left (ParseFail _ msg) -> fail msg
    Right parsed -> pure parsed
  (leadTy , r2) <-
    case break isLBrace r1 of
      (_, []) -> fail "Ran out of input parsing leading type in quasiquote Fortran"
      (tyToks, lBrace : rest2) -> pure (head tyToks, clearRBrace rest2)

  -- Split off the leading type's tokens
  (tyToks, rest2) <-
    case break openBrace rest1 of
      (_, []) -> fail "Ran out of input parsing leading type in quasiquote"
      (tyToks, _ : rest2) -> pure (tyToks, init $ tail rest2)

  -- Parse leading type
  leadingTy <-
    case parseFromToks tyToks of
      Left (ParseFail _ msg) -> fail msg
      Right parsed -> pure parsed

  -- Parse body of quasiquote
  (bodyToks, vars) <- parseBody [] [] rest2
  (bodyTF, varsF) <- parseBodyF [] [] r2
  let qq = (QQParse leadingTy bodyToks vars)
  let fq = (FQParse leadTy bodyTF varsF)

  -- Done!
  return qq

  where
    -- Parse the body of the quasiquote
    parseBody :: [SpTok] -> [(String, Ty Span)] -> [SpTok]
              -> Q ([SpTok], [(String, Ty Span)])
    parseBody toks vars rest1
      = case rest1 of
          [] -> pure (reverse toks, vars)
          (Spanned Dollar _             :
           Spanned (OpenDelim Paren) _  :
           Spanned (IdentTok i) _       :
           Spanned (CloseDelim Paren) _ : rest2) -> do
             let i' = name i
             parseBody (pure (IdentTok i) : toks) vars rest2

          (Spanned Dollar _            :
           Spanned (OpenDelim Paren) _ :
           Spanned (IdentTok i) _      :
           Spanned Colon _             : rest2) -> do

            let i' = name i
            -- Parse the rest of the escape
            (t1, rest3) <- parseEscape [] 1 rest2
            newVars <- case lookup i' vars of
                         Nothing -> pure ((i', t1) : vars)
                         Just t2 | void t1 == void t2 -> pure vars
                                 | otherwise -> fail (dupMsg i' t1 t2)

            -- Continue parsing
            parseBody (pure (IdentTok i) : toks) newVars rest3

          (tok : rest2) -> parseBody (tok : toks) vars rest2

    -- Add it to 'vars' if it isn't a duplicate
    dupMsg ia t1a t2a = concat [ "Variable `", ia, ": ", renderType t1a
                               , "' has already been given type `"
                               , renderType t2a, "'"
                               ]
    -- Parse the part of escapes like @$(x: i32)@ that comes after the @:@.
    parseEscape :: [SpTok] -> Int -> [SpTok] -> Q (Ty Span, [SpTok])
    parseEscape toks p rest1
      = case rest1 of
          [] -> fail "Ran out of input while parsing variable escape"
          tok : rest2
            | openParen tok           -> parseEscape (tok : toks) (p+1) rest2
            | closeParen tok && p > 1 -> parseEscape (tok : toks) (p-1) rest2
            | not (closeParen tok)    -> parseEscape (tok : toks) p     rest2
            | otherwise -> case parseFromToks (reverse toks) of
                             Left (ParseFail _ msg) -> fail msg
                             Right parsed           -> pure (parsed, rest2)


-- | Utility function for parsing AST structures from listf of spanned tokens
parseFromToks :: Parse a => [SpTok] -> Either ParseFail a
parseFromToks toks = execParserTokens parser toks initPos

-- | Identifies an open brace token
openBrace :: SpTok -> Bool
openBrace (Spanned (OpenDelim Brace) _) = True
openBrace _ = False

-- | Identifies an open paren token
openParen :: SpTok -> Bool
openParen (Spanned (OpenDelim Paren) _) = True
openParen _ = False

-- | Identifies a close paren token
closeParen :: SpTok -> Bool
closeParen (Spanned (CloseDelim Paren) _) = True
closeParen _ = False
