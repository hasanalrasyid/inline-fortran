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

import Language.Fortran.Syntax        ( Token(..), Delim(..), Ty(..) )
import Language.Fortran.Parser
import Language.Rust.Data.Position ( Spanned(..), Position(..), Span(..) )
import Language.Rust.Data.Ident    ( Ident(..) )

import Language.Haskell.TH         ( Q, runIO )

import Control.Monad               ( void )

-- All the tokens we deal with are 'Spanned'...
type SpTok = Spanned Token

-- | Result of parsing a quasiquote. Quasiquotes are of the form
-- @<ty> { <block> }@ where the @<block>@ possibly contains escaped arguments
-- in the form of @$(<ident>: <ty>)@.
data RustQuasiquoteParse = QQParse

  -- | leading type (corresponding to the return type of the quasiquote)
  { ty :: Ty Span

  -- | body tokens, with @$(<ident>: <ty>)@ escapes replaced by just @ident@
  , body :: [SpTok]

  -- | escaped arguments
  , variables :: [(String, (Ty Span, String))]

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
  runIO $ putStrLn $ "stream: " ++ show stream

  -- Lex the quasiquote tokens
  rest1 <-
    case execParser lexer stream initPos of
      Left (ParseFail _ msg) -> fail msg
      Right parsed -> pure parsed
  -- dari sini rest1 sudah punya newline setelah openParen
  runIO $ putStrLn $ "rest1: " ++ show rest1

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
  (bodyToks, vars) <- parseBody [] [] rest1

  -- Done!
--  let dummy = snd $ head vars
  let dummy = Never (Span NoPosition NoPosition)
--  runIO $ putStrLn $ "dummy: " ++ show dummy
  pure (QQParse dummy bodyToks vars)

  where
    -- Parse the body of the quasiquote
    parseBody toks vars rest = do
        case rest of
          [] -> pure (reverse toks, vars)

          (t@(Spanned (OpenDelim Paren) _)         :
           tt@(Spanned t2 _)                          : rst2) -> do
            runIO $ putStrLn $ "parseBody OpenDelim Paren:" ++ show rst2
            runIO $ do
              case t2 of
                TNewLine -> do
                  putStrLn $ "parseBody afterthat :"
                  print t2
                _ -> putStrLn "unknown token"
            parseBody (t:tt:toks) vars rst2
          (Spanned Dollar _            :
           Spanned (OpenDelim Paren) _ :
           Spanned (IdentTok i) _      :
           Spanned Colon _             :
           Spanned (IdentTok intent) _ :
           Spanned Colon _             : rst2) -> do
            -- Parse the rest of the escape
            (t1, rst3) <- parseEscape [] 1 rst2
--            runIO $ putStrLn $ "parseBody $(i) rst2: " ++ show rst2
--            runIO $ putStrLn $ "parseBody $(i)   t1: " ++ show t1
--            runIO $ putStrLn $ "parseBody $(i) rst3: " ++ show rst3

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

            -- Continue parsing
            parseBody (pure (IdentTok i) : toks) newVars rst3

          (tok : rst2) -> parseBody (tok : toks) vars rst2

    -- Parse the part of escapes like @$(x: i32)@ that comes after the @:@.
    parseEscape :: [SpTok] -> Int -> [SpTok] -> Q (Ty Span, [SpTok])
    parseEscape toks p rst1 = do
--        runIO $ putStrLn $ "parseEscape toks p rst1: " ++ show toks ++ show p ++ show rst1
        case rst1 of
          [] -> fail "Ran out of input while parsing variable escape"
          tok : rst2
            | openParen tok           -> parseEscape (tok : toks) (p+1) rst2
            | closeParen tok && p > 1 -> parseEscape (tok : toks) (p-1) rst2
            | not (closeParen tok)    -> parseEscape (tok : toks) p     rst2
            | otherwise -> do
--                runIO $ putStrLn $ "parseEscape otherwise: " ++ show toks
                case parseFromToks (reverse toks) of
                             Left (ParseFail _ msg) -> fail msg
                             Right parsed           -> pure (parsed, rst2)


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

