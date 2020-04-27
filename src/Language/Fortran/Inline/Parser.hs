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

module Language.Fortran.Inline.Parser where

import Language.Fortran.Inline.Pretty ( renderType )

import Language.Fortran.Syntax        ( Token(..), Delim(..), Ty(..), LitTok(..) )
import Language.Fortran.Parser
import Language.Rust.Data.Position ( Spanned(..), Position(..), Span(..) )
import Language.Rust.Data.Ident    ( Ident(..) )

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
  REVISED:
  we need leading type, we switch openbrace with ::
-}
  -- Split off the leading type's tokens
  (tyToks, rest2) <-
    case break doubleColon rest1 of
      (_, []) -> fail "Ran out of input parsing leading type in quasiquote"
      (tyToks, _ : rest2) -> pure (tyToks, rest2)
  -- Parse leading type
  leadingTy <-
    case parseFromToks tyToks of
      Left (ParseFail _ msg) -> fail msg
      Right parsed -> pure parsed
--

  -- Parse body of quasiquote
  (bodyToks, vars) <- parseBody [] [] rest2

  -- Done!
--  let dummy = snd $ head vars
--let
--  dummy :: Ty Span
--  dummy = Never (Span NoPosition NoPosition) -- this should be return value
--  bodyToks <- fmap (rearrange) $ constructFortran $  takeWhile' bodyToks'
  (locVars,bodyToks2) <- takeWhile' bodyToks
--  runIO $ putStrLn $ "dummy: " ++ show  bodyToks
  bodyToks' <- cekLitTok [] bodyToks2
  pure (QQParse leadingTy vars locVars bodyToks')
  where
    cekLitTok r [] = pure $ reverse r
    cekLitTok r (t:ts) = do
--      runIO $ putStrLn $ "cekLitTok: " ++ show t
      let tt = (flip map) t $ \t1 ->  case t1 of
                                l@(Spanned (LiteralTok IntegerTok{} _) _) ->
                                  case (reads (show l) :: [(Float,String)]) of
                                    (_,""):_ -> t1
                                    _        -> head $ makeLiteral [] [l]
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
    parseBody toks vars [] =pure (reverse toks, vars)
    parseBody toks vars (e@(Spanned Exclamation _): d@(Spanned Dollar _): rst2) =
      parseBody (d:e:toks) vars rst2
    parseBody toks vars (Spanned Dollar _  : rst2) = do
      runIO $ putStrLn "parseBody: Dollar"
      (rst3, iD@(v,i,r)) <- takeDollar Nothing [] rst2
      runIO $ putStrLn $ "inDollar:" ++ show iD
      newVars <- parseV vars v i r
      parseBody (v:toks) newVars rst3
    parseBody toks vars (r:rst2) =
      parseBody (r:toks) vars rst2

    parseFType toks =
      case (parseFromToks toks) of
        Left (ParseFail _ msg) -> fail $ "parseFType: " ++ msg ++ " " ++ show toks
        Right parsed           -> do
                                     pure parsed

    parseV vars (Spanned (IdentTok i) _) (Spanned (IdentTok intent) _) rst2 = do
      -- Parse the rest of the escape
      runIO $ putStrLn $ "parseV: rst2: " ++ show rst2
      t1 <- case rst2 of
              FVarBase t -> parseFType t
              FVarString _ -> pure $ (FString nullSpan)
              FVarArray t (Spanned (LiteralTok (IntegerTok n) _) _) -> do
                ty1 <- parseFType t
                let dim = read n
                pure $ FArray dim ty1 nullSpan
              _ -> fail $ "parseV: t1: error on case rst2"
      runIO $ putStrLn $ "parseV: t1: " ++ show t1
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
      pure newVars
    parseV _ _ _ _ = return $ fail $ "parseV: sumthin wrong"

    processVITDP [] = return (nullSpTok,nullSpTok,Nothing)
    processVITDP (_:vs) = do
      let (v:tIntent:res) = wordsBy isColon $ init vs
      r <- go res
      runIO $ putStrLn $ "processVITDP: r: " ++ show r
      return (head v, head tIntent,r)
        where go [] = pure Nothing
              go (t:[]) = pure $ Just (t,Nothing)
              go (t:di:_) = pure $ Just (t,Just $ head di)
--              go _ = fail $ "error processVITDP"

    takeDollar :: Maybe Int -> [SpTok]-> [SpTok] -> Q ([SpTok],(SpTok,SpTok,FVar))
    takeDollar (Just 0) cs rs = do
      runIO $ putStrLn $ "takeDollar Just 0" ++ show cs
      fVar <- case reverse cs of
               vitP@((Spanned (OpenDelim Paren) _):_) -> do
                  runIO $ putStrLn $ "!takeDollar: OpenDelim rs" ++ show vitP
                  (v,i, Just (t,_)) <- processVITDP vitP
                  return $ (v,i,FVarBase t)
               ((Spanned (IdentTok (Ident "str" _ _)) _):vidP) -> do
                  runIO $ putStrLn $ "!takeDollar: str si: " ++ show vidP
                  (v,i, Just (d,_)) <- processVITDP vidP
                  return $ (v,i,FVarString $ head d)
               ((Spanned (IdentTok (Ident "vec" _ _)) _):vitdP) -> do
                  runIO $ putStrLn $ "!takeDollar: vec rs" ++ show vitdP
                  (v,i, Just (t,Just d)) <- processVITDP vitdP
                  return $ (v,i,FVarArray t d)
               _ -> do
                 runIO $ putStrLn $ "!takeDollar: ERROR " ++ show rs
                 return $ (Spanned Eof nullSpan, Spanned Eof nullSpan, ErrFVar)
      return (rs,fVar)

    takeDollar Nothing cs (r:rs)
      | openParen r = takeDollar (Just 1) (r:cs) rs
      | otherwise = takeDollar Nothing (r:cs) rs
    takeDollar j@(Just i) cs (r:rs)
      | openParen r = takeDollar (Just $ 1 + i) (r:cs) rs
      | closeParen r = takeDollar (Just $ i - 1) (r:cs) rs
      | otherwise = takeDollar j (r:cs) rs
    takeDollar _ _ [] = fail $ "takeDollar for []"
  {-
    parseBody' parCount toks vars rest = do
        case rest of
          [] -> if parCount /= 0 then fail $ "Too much Parenthesis==" -- ++ show parCount ++ "==toks==" ++ (show $ reverse toks) ++ "==vars==" ++ show vars
                                 else pure (reverse toks, vars)
          (Spanned t@(OpenDelim Paren) _ : rst2) -> do
--            runIO $ putStrLn $ "Paren132:" ++ show t ++ "==" ++ show parCount ++ "==" ++ (show $ head rst2)
            parseBody (parCount+1) (pure t : toks) vars rst2
          (Spanned t@TNewLine _ :
           rst2@(Spanned Ampersand _ : _)) -> do
            parseBody' parCount (pure t:toks) vars rst2
          (Spanned t@TNewLine _ : rst2) -> do
            if parCount > 0 then parseBody' parCount         toks  vars rst2
                            else parseBody' parCount (pure t:toks) vars rst2
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
             parseBody' parCount (pure (IdentTok i) : toks) newVars rst3

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
                 parseBody' parCount (pure (IdentTok i) : toks) newVars rst3
               _ -> fail $ "error $" ++ vec ++ " is not yet implemented"

          (Spanned Dollar _            :
           Spanned (OpenDelim Paren) _ :
           Spanned (IdentTok i) _      :
           Spanned Colon _             :
           Spanned (IdentTok intent) _ :
           Spanned Colon _             : rst2) -> do
             (newVars,rst3) <- parseVars Nothing vars i intent rst2
             parseBody' parCount (pure (IdentTok i) : toks) newVars rst3

          (tok : rst2) -> parseBody' parCount (tok : toks) vars rst2
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
        case rst1 of
          [] -> fail "Ran out of input while parsing variable escape"
          tok : rst2
            | isColon tok           -> parseEscape (tok : toks) (p) rst2
            | openParen tok           -> parseEscape (tok : toks) (p+1) rst2
            | closeParen tok && p > 1 -> parseEscape (tok : toks) (p-1) rst2
            | not (closeParen tok)    -> parseEscape (tok : toks) p     rst2
            | otherwise -> do
                runIO $ putStrLn $ unwords
                  ["parseEscape toks p rst1: ",show $ reverse toks]
                case (parseFromToks $ reverse toks) of
                             Left (ParseFail _ msg) -> fail $ "parseEscape: " ++ msg
                             Right parsed           -> do
                               pure (parsed, rst2)
    -}


--strFType = FType (mkIdent "character") (TupExpr [] [] (Span (Position 1 227 54) (Position 10 227 63))) (Span (Position 1 227 54) (Position 10 227 63))
strFType :: Ty Span
strFType = FString nullSpan

nullSpTok :: SpTok
nullSpTok = Spanned Eof nullSpan

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

nullSpan :: Span
nullSpan = Span NoPosition NoPosition

-- | Identifies an open paren token
openParen :: SpTok -> Bool
openParen (Spanned (OpenDelim Paren) _) = True
openParen _ = False

doubleColon :: SpTok -> Bool
doubleColon (Spanned ModSep _) = True
doubleColon _ = False

-- | Identifies a close paren token
closeParen :: SpTok -> Bool
closeParen (Spanned (CloseDelim Paren) _) = True
closeParen _ = False

isColon :: SpTok -> Bool
isColon (Spanned Colon _) = True
isColon _ = False

data FVar = ErrFVar
          | FVarString SpTok -- length
          | FVarBase [SpTok] -- type
          | FVarArray [SpTok] SpTok -- type dimLength
          deriving Show
