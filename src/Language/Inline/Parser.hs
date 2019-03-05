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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Language.Inline.Parser where

import Language.Inline.Pretty ( renderType )
import Language.Inline.Utils

import Language.Rust.Syntax        ( Token(..), Delim(..), Ty(..))
import Language.Rust.Data.InputStream

--import qualified Language.Lexer.FreeForm as Free ( collectFreeTokens, Token(..), lexer', initParseState, LexAction(..),AlexInput(..),AlexReturn(..),resetLexeme,scActual,normaliseStartCode,User(..),alexScanUser,StartCode(..),StartCodeStatus(..))
import qualified Language.Fortran.Lexer.FreeForm as Free
import qualified Language.Inline.Lexer.FreeForm as L
import qualified Data.ByteString.Char8 as B8
import qualified Language.Fortran.ParserMonad as FPM
import qualified Language.Fortran.Util.Position as FP
import qualified Language.Fortran.AST               as F
import Control.Monad.State (get)

import Language.Rust.Parser
import Language.Rust.Data.Position ( Span(..), Spanned(..), Position(..) )
import Language.Rust.Data.Ident    ( Ident(..) )

import Language.Haskell.TH         ( Q, runIO )

import Control.Monad               ( void )

import Language.Fortran.Util.ModFile ( emptyModFiles )

import Data.Foldable (traverse_)
import qualified Language.Inline.Parser.ParseMonad as FIPM
import qualified Language.Inline.Parser.Fortran95 as PF95
-- All the tokens we deal with are 'Spanned'...
type SpTok = Spanned Token
type SpLTok = Spanned L.Token

-- | Result of parsing a quasiquote. Quasiquotes are of the form
-- @<ty> { <block> }@ where the @<block>@ possibly contains escaped arguments
-- in the form of @$(<ident>: <ty>)@.
data FortQuasiquoteParse = FQParse

  -- | leading type (corresponding to the return type of the quasiquote)
  { tyF :: [L.Token]

  -- | body tokens, with @$(<ident>: <ty>)@ escapes replaced by just @ident@
  , bodyF :: [L.Token]

  -- | escaped arguments
  , variablesF :: [(String, L.Token)]

  } deriving (Show)

data RustQuasiquoteParse = QQParseR

  -- | leading type (corresponding to the return type of the quasiquote)
  { ty :: Ty Span

  -- | body tokens, with @$(<ident>: <ty>)@ escapes replaced by just @ident@
  , body :: [SpTok]

  -- | escaped arguments
  , variables :: [(String, Ty Span)]

  } | QQParseF
  { tyQF :: (Maybe (F.TypeSpec F.A0))

  -- | body tokens, with @$(<ident>: <ty>)@ escapes replaced by just @ident@
  , bodyQF :: [L.Token]

  -- | escaped arguments
  , variablesQF :: [(String, L.Token)]

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
  let a = L.collectFreeTokens FPM.Fortran95 $ B8.pack s
  in if | a == [] -> Left (ParseFail (Position 0 0 0) "error execParserFortran")
        | otherwise -> Right a

isLBraceS :: Spanned L.Token -> Bool
isLBraceS (Spanned s _) = isLBrace s

isLBrace :: L.Token -> Bool
isLBrace (L.TLBrace _) = True
isLBrace _ = False

isRBrace :: L.Token -> Bool
isRBrace (L.TRBrace _) = True
isRBrace _ = False
-- Parse the body of the quasiquote
--parseBodyF :: [SpLTok] -> [(String, Ty Span)] -> [SpLTok]
--          -> Q ([SpLTok], [(String, Ty Span)])
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

clearFrom isWhat l = reverse $ tail $ dropWhile (not . isWhat) $ reverse l

parseFQ :: String -> Q FortQuasiquoteParse
parseFQ input = do
  let lexer = lexTokens lexNonSpace
  let stream = inputStreamFromString input
  -- Lex the quasiquote tokens

  rest1 <-  case execParser lexer stream initPos of
      Left (ParseFail _ msg) -> fail msg
      Right (parsed :: [Spanned Token]) -> pure parsed

  r1 <- case execParserFortran input of
    Left (ParseFail _ msg) -> fail msg
    Right (parsed :: [L.Token]) -> pure parsed
  (tyToks , r2) <-
    case break isLBrace r1 of
      (_, []) -> fail "====Ran out of input parsing leading type in quasiquote Fortran"
      (tyToks, lBrace : rest2) -> pure (tyToks, clearFrom isRBrace rest2)
  {-
    -- Parse leading type
  leadTy <-
    case parseFromFToks tyToks of
      Left (ParseFail _ msg) -> fail msg
      Right parsed -> pure parsed
-}

  -- Split off the leading type's tokens

  -- Parse leading type

  -- Parse body of quasiquote
  (bodyTF, varsF) <- parseBodyF [] [] r2
  let fq = (FQParse tyToks bodyTF varsF)

  -- Done!
  return fq

-- spanLToken :: L.Token -> Spanned L.Token
spanLToken a = let (FP.SrcSpan i j) = FP.getSpan a
                   convPos (FP.Position ab c r _) = Position ab r c
                   x = Position 0 0 0
                in Spanned a (Span (convPos i) (convPos j))
getLToken (Spanned a _) = a

parseQQ :: String -> Q RustQuasiquoteParse
parseQQ input = do
  let lexer = lexTokens lexNonSpace
  let stream = inputStreamFromString input
  -- Lex the quasiquote tokens
  let input1 = unlines $
                  "    double precision {":
                  "      double precision a,b,c,eps":
                  "      a = 4.0d0/3.0d0":
                  "   10 b = a - 1.0d0":
                  "      c = b + b + b":
                  "      eps = dabs(c-1.0d0)":
                  "      if (eps .eq. 0.0d0) go to 10":
                  "      $(ret) = eps*dabs(x)":
                  "      return":
                  "      }":[]

  rest1 <-  case execParser lexer stream initPos of
      Left (ParseFail _ msg) -> fail msg
      Right parsed -> pure parsed
  r1' <- case execParserFortran input1 of
    Left (ParseFail _ msg) -> fail msg
    Right parsed -> pure parsed
  let (r1 :: [Spanned L.Token]) = map spanLToken r1'
  debugIt "===test ===" [r1]
  (tyToksF, r2) <-
    case break openBrace r1 of
      (_, []) -> fail "ParseQQ: Ran out of input parsing leading type in quasiquote Fortran"
      (tyToks, lBrace : rest2) -> pure (tyToks, rest2)

  -- Split off the leading type's tokens
  (tyToks, rest2) <-
    case break openBrace rest1 of
      (_, []) -> fail "Ran out of input parsing leading type in quasiquote"
      (tyToks, _ : rest2) -> pure (tyToks, init $ tail rest2)
  -- Parse leading type
  leadingTy <-
    case parseFromToks tyToks of
      Left (ParseFail _ msg) -> fail msg
      Right (parsed :: Ty Span) -> pure parsed

  leadTy <- pure $ tyParser $ B8.pack $ head $ lines input1
  runIO $ do
    putStrLn $ show leadingTy
    putStrLn $ show leadTy


  -- Parse body of quasiquote
  (bodyToks, vars) <- parseBody [] [] rest2
  (bodyTF, varsF) <- parseBodyF [] [] $ map getLToken r2
  let qq = (QQParseR leadingTy bodyToks vars)
  let qf = (QQParseF leadTy bodyTF varsF)
  debugIt "qf ===" [qf]

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
-- parseFromToks :: Parse a => [SpTok] -> Either ParseFail a
-- parseFromToks toks = execParserTokens parser toks initPos

-- | Identifies an open brace token
--openBrace :: SpTok -> Bool

class CommonToken a where
  openBrace :: a -> Bool


instance CommonToken (Spanned Token) where
  openBrace (Spanned (OpenDelim Brace) _) = True
  openBrace _ = False

parseFromToks :: Parse b => [SpTok] -> Either ParseFail b
parseFromToks toks = execParserTokens parser toks initPos

sParser :: B8.ByteString -> F.Statement F.A0
sParser sourceCode =
  FPM.evalParse PF95.statementParser $ L.initParseState sourceCode FPM.Fortran95 "<unknown>"

--tyParser :: B8.ByteString -> Maybe (F.TypeSpec F.A0)
tyParser sourceCode =
  FPM.evalParse PF95.typeParser $ L.initParseState sourceCode FPM.Fortran95 "<unknown>"

  {-
kadal = go =<< PF95.typeParser
  where go :: Maybe (F.TypeSpec F.A0) -> L.LexAction (Maybe (F.TypeSpec F.A0))
        go Nothing = pure Nothing
        go a = pure a
-}

parseFromTyToksExec x =
  FPM.execParse PF95.typeParser x
parseFromTyToksEval x =
  FPM.evalParse PF95.typeParser x


instance CommonToken (Spanned L.Token) where
  openBrace (Spanned (L.TLBrace _) _) = True
  openBrace _ = False

-- openBrace (Spanned (L.TLBrace _) _) = True
-- openBrace (Spanned (OpenDelim Brace) _) = True
-- openBrace _ = False

-- | Identifies an open paren token
openParen :: SpTok -> Bool
openParen (Spanned (OpenDelim Paren) _) = True
openParen _ = False

-- | Identifies a close paren token
closeParen :: SpTok -> Bool
closeParen (Spanned (CloseDelim Paren) _) = True
closeParen _ = False

collectTokens2 :: forall a b . (FP.Loc b, FPM.Tok a, FPM.LastToken b a, Show a) => FPM.Parse b a a -> FPM.ParseState b -> [a]
collectTokens2 lexer initState =
    FPM.evalParse (_collectTokens initState) undefined
  where
    _collectTokens :: (FP.Loc b, FPM.Tok a, FPM.LastToken b a, Show a) => FPM.ParseState b -> FPM.Parse b a [a]
    _collectTokens state = do
      let (_token, _state) = FPM.runParseUnsafe lexer state
--      let _mTy = FPM.runParseUnsafe PF95.typeParser state
      if FPM.eofToken _token then return [_token]
                             else do
                               _tokens <- _collectTokens _state
                               return $ _token:_tokens

