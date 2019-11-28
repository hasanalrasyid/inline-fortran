{-|
Module      : Language.Rust.Inline.Parser
Description : Parser for Quasiquotes
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Rust.Inline.Parser where

import Language.Rust.Inline.Pretty ( renderType )

import Language.Fortran.Syntax        ( Token(..), Delim(..), Ty(..), LitTok(..), Expr(..))
import Language.Fortran.Parser
import Language.Rust.Data.Position ( Spanned(..), Position(..), Span(..), Located(..) )
import Language.Rust.Data.Ident    ( Ident(..), mkIdent )

import Language.Haskell.TH         ( Q, runIO )

import Control.Monad               ( void )
import Data.List.Split ( wordsBy )
import Data.List
import Data.Char

-- All the tokens we deal with are 'Spanned'...
type SpTok = Spanned Token

-- | Result of parsing a quasiquote. Quasiquotes are of the form
-- @<ty> { <block> }@ where the @<block>@ possibly contains escaped arguments
-- in the form of @$(<ident>: <ty>)@.
data RustQuasiquoteParse = QQParse

  -- | leading type (corresponding to the return type of the quasiquote)
  { ty :: Ty Span

  -- | escaped arguments
  , variables :: [(String, (Ty Span, String))]
  , locVar :: Maybe Int
  -- | body tokens, with @$(<ident>: <ty>)@ escapes replaced by just @ident@
  , body :: [[SpTok]]

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
parseQQ :: String -> Q RustQuasiquoteParse
parseQQ input = do

  let lexer = lexTokens lexNonSpace
  let stream = inputStreamFromString input
  -- dari sini stream tak punya newline setelah openParen
--  runIO $ putStrLn $ "stream: " ++ show stream

  -- Lex the quasiquote tokens
  rest1 <-
    case execParser lexer stream initPos of
      Left (ParseFail _ msg) -> fail msg
      Right parsed -> pure parsed
  -- dari sini rest1 sudah punya newline setelah openParen
--  runIO $ putStrLn $ "rest1: " ++ show rest1

  {- No need for leading type, we can go straight to body
      it means body need no brace
  -- Split off the leading type's tokens
  (tyToks, rest2) <-
    case break openBrace rest1 of
      (_, []) -> fail "Ran out of input parsing leading type in quasiquote"
      (tyToks, brace : rest2) -> pure (tyToks, brace : rest2)

  -- Parse leading type
  leadingTy <-
    case parseFromToks tyToks of
      Left (ParseFail _ msg) -> fail msg
      Right parsed -> pure parsed
--}

  -- Parse body of quasiquote
  (bodyToks, vars) <- parseBody 0 [] [] rest1

  -- Done!
--  let dummy = snd $ head vars
  let dummy = Never (Span NoPosition NoPosition)
--  bodyToks <- fmap (rearrange) $ constructFortran $  takeWhile' bodyToks'
  (locVars,bodyToks) <- takeWhile' bodyToks
--  runIO $ putStrLn $ "dummy: " ++ show  bodyToks
  bodyToks' <- cekLitTok [] bodyToks
  pure (QQParse dummy vars locVars bodyToks')
  where
    cekLitTok r [] = pure $ reverse r
    cekLitTok r (t:ts) = do
--      runIO $ putStrLn $ "cekLitTok: " ++ show t
      let tt = (flip map) t $ \t1 ->  case t1 of
                                l@(Spanned (LiteralTok IntegerTok{} _) _) ->
                                  case (reads (show l) :: [(Float,String)]) of
                                    (_,""):_ -> t1
                                    otherwise -> head $ makeLiteral [] [l]
                                _ -> t1
      cekLitTok (tt:r) ts
    takeWhile' as = do
      let rs = wordsBy f as
          nl = filter f as
          z1 = filter (not . f3) $ zipWith (\a b -> a ++ [b]) rs nl
      pure $ (fmap (+1) $ findIndex f4 z1,z1)
        where
          f (Spanned TNewLine _) = True
          f _                    = False
          f2 (Spanned (IdentTok s) _) = "implicit" == (map toLower $ name s)
          f2 (Spanned ModSep _) = True
          f2  _ = False
          f3 ((Spanned Pound _):_) = True
          f3 _ = False
          f4 x = case findIndex f2 x of
                   Just _ -> True
                   Nothing -> False
    -- Parse the body of the quasiquote
    parseBody parCount toks vars rest = do
        case rest of
          [] -> if parCount /= 0 then fail $ "Too much Parenthesis==" -- ++ show parCount ++ "==toks==" ++ (show $ reverse toks) ++ "==vars==" ++ show vars
                                 else pure (reverse toks, vars)
          (Spanned t@(OpenDelim Paren) _ : rst2) -> do
--            runIO $ putStrLn $ "Paren132:" ++ show t ++ "==" ++ show parCount ++ "==" ++ (show $ head rst2)
            parseBody (parCount+1) (pure t : toks) vars rst2
          (Spanned t@TNewLine _ :
           rst2@(Spanned Ampersand _ : _)) -> do
            parseBody parCount (pure t:toks) vars rst2
          (Spanned t@TNewLine _ : rst2) -> do
            if parCount > 0 then parseBody parCount         toks  vars rst2
                            else parseBody parCount (pure t:toks) vars rst2
          (Spanned t@(CloseDelim Paren) _ : rst2) -> do
--            runIO $ putStrLn $ "Paren141:" ++ show t ++ "==" ++ (show $ head rst2)
            parseBody (parCount-1) (pure t : toks) vars rst2

          (Spanned Dollar _            :
           s@(Spanned (IdentTok (Ident "str" _ _)) _)      :
           Spanned (OpenDelim Paren) _ :
           Spanned (IdentTok i) _      :
           Spanned Colon _             :
           Spanned (IdentTok intent) _ : rst2) -> do
             (newVars,rst3) <- parseVars (Just ("str",s)) vars i intent rst2
             parseBody parCount (pure (IdentTok i) : toks) newVars rst3

          (Spanned Dollar _            :
           Spanned (IdentTok v) _      :
           Spanned (OpenDelim Paren) _ :
           Spanned (IdentTok i) _      :
           Spanned Colon _             :
           Spanned (IdentTok intent) _ :
           Spanned Colon _             : rst2) -> do
             let vec = name v
             case vec of
               "vec" -> do
                 (newVars,rst3) <- parseVars Nothing vars i intent rst2
                 parseBody parCount (pure (IdentTok i) : toks) newVars rst3
               _ -> fail $ "error $" ++ vec ++ " is not yet implemented"

          (Spanned Dollar _            :
           Spanned (OpenDelim Paren) _ :
           Spanned (IdentTok i) _      :
           Spanned Colon _             :
           Spanned (IdentTok intent) _ :
           Spanned Colon _             : rst2) -> do
             (newVars,rst3) <- parseVars Nothing vars i intent rst2
             parseBody parCount (pure (IdentTok i) : toks) newVars rst3

          (tok : rst2) -> parseBody parCount (tok : toks) vars rst2

    parseVars strStat vars i intent rst2@(_:r2) = do
      -- Parse the rest of the escape
      (t1, rst3) <- case strStat of
                      Nothing -> parseEscape [] 1 rst2
                      Just ("str",s) -> pure (strFType,r2)
                      Just (s,_) -> fail $ "Not implemented " ++ s
      runIO $ putStrLn $ "parseVars:t1: " ++ show t1
      -- Add it to 'vars' if it isn't a duplicate
      let i' = name i
      let dupMsg t2 = concat [ "Variable `", i', ": ", renderType t1
                              , "' has already been given type `"
                              , renderType t2, "'"
                              ]
      let intent' = name intent
      newVars <- case lookup i' vars of
                   Nothing -> pure ((i', (t1,intent')) : vars)
                   Just (t2,_) | void t1 == void t2 -> pure vars
                               | otherwise -> fail (dupMsg t2)
      pure (newVars,rst3)

            -- Continue parsing
    -- Parse the part of escapes like @$(x: i32)@ that comes after the @:@.
    parseEscape :: [SpTok] -> Int -> [SpTok] -> Q (Ty Span, [SpTok])
    parseEscape toks p rst1 = do
--        runIO $ putStrLn $ "parseEscape toks p rst1: " ++ show toks ++ show p ++ show rst1
        case rst1 of
          [] -> fail "Ran out of input while parsing variable escape"
          tok : rst2
            | isColon tok           -> parseEscape (tok : toks) (p) rst2
              {-
              let parsedTy = parseFromToks (reverse toks)
              (tupTy,rst3) <- parseEscapeExpr [] p rst2
              runIO $ putStrLn $ "tupTy: " ++ show tupTy
              case parsedTy of
                Left (ParseFail _ msg) -> fail $ "parseEscape parsedTy: " ++ msg
                Right ty -> pure(ty, rst3)
                -}

            | openParen tok           -> parseEscape (tok : toks) (p+1) rst2
            | closeParen tok && p > 1 -> parseEscape (tok : toks) (p-1) rst2
            | not (closeParen tok)    -> parseEscape (tok : toks) p     rst2
            | otherwise -> do
                case (parseFromToks $ reverse toks) of
                             Left (ParseFail _ msg) -> fail $ "parseEscape: " ++ msg
                             Right parsed           -> do
                               pure (parsed, rst2)


--strFType = FType (mkIdent "character") (TupExpr [] [] (Span (Position 1 227 54) (Position 10 227 63))) (Span (Position 1 227 54) (Position 10 227 63))
strFType = FString nullSpan

makeLiteral :: [SpTok] -> [SpTok] -> [SpTok]
makeLiteral r [] = r
makeLiteral [] ((Spanned t s):rs) =
  makeLiteral
    [(Spanned (LiteralTok (ByteStrTok $ show t) Nothing) s)] rs
makeLiteral (r:_) ((Spanned t s):rs) =
  makeLiteral
    [(Spanned (LiteralTok (ByteStrTok $ show r ++ show t) Nothing) s)] rs

-- | Utility function for parsing AST structures from listf of spanned tokens
parseFromToks :: Parse a => [SpTok] -> Either ParseFail a
parseFromToks toks = execParserTokens parser toks initPos

-- | Identifies an open brace token
openBrace :: SpTok -> Bool
openBrace (Spanned (OpenDelim Brace) _) = True
openBrace _ = False

nullSpan = Span NoPosition NoPosition

-- | Identifies an open paren token
openParen :: SpTok -> Bool
openParen (Spanned (OpenDelim Paren) _) = True
openParen _ = False

-- | Identifies a close paren token
closeParen :: SpTok -> Bool
closeParen (Spanned (CloseDelim Paren) _) = True
closeParen _ = False

isColon :: SpTok -> Bool
isColon (Spanned Colon _) = True
isColon _ = False
