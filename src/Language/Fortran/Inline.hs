{-|
Module      : Language.Rust.Inline
Description : Quasiquotes for writing Rust code inline in Haskell
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Fortran.Inline (
  -- * Overview
  --
  -- $overview
  --
  -- * Quasiquoters
  --
  -- $quasiquoters
  --
  -- ** Safe
  --
  -- $safe
  rust,
  rustIO,
  fortIO,
  -- ** Unsafe
  --
  -- $unsafe
  rustUnsafe,
  rustUnsafeIO,
  -- ** Interruptible
  --
  -- $interruptible
  rustInterruptible,
  rustInterruptibleIO,

  -- * Contexts
  Context(..),
  RType,
  HType,
  -- ** Using and defining contexts
  setCrateModule,
  setCrateRoot,
  extendContext,
  singleton,
  mkContext,
  lookupRTypeInContext,
  getRTypeInContext,
  lookupHTypeInContext,
  getHTypeInContext,
  -- ** Built-in contexts
  basic,
  fVectors,
  vectors,
  libc,
  ghcUnboxed,
  functions,
  pointers,
  prelude,
  -- ** Marshalling
  readOnlyVectors,
  unsafeWithVectors,
  withPtr,
  withPtrsN,
  genWithPtrs,
  with,
  alloca,
  free,
  new,
  withFunPtr,
  newFunPtr,
  unFunPtr,
  freeHaskellFunPtr,
  withArrayLen,
  withStorableArrayLen,
  newArray,
  withByteString,
  unsafeLocalState,
  mkStorable,
--  mkReprC,

  -- * Top-level Rust items
 -- externCrate,
) where

import Data.List.Split (chunksOf, splitOn)
import Language.Fortran.Inline.Context
import Language.Fortran.Inline.Context.Prelude  ( prelude )
import Language.Fortran.Inline.Internal
import Language.Fortran.Inline.Marshal
import Language.Fortran.Inline.Parser
import Language.Fortran.Inline.Pretty
import Language.Fortran.Inline.TH.Storable      ( mkStorable )
--import Language.Rust.Inline.TH.ReprC         ( mkReprC )

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote             ( QuasiQuoter(..) )
import Language.Haskell.TH                   ( pprParendType )

import Foreign.Marshal.Utils                 ( with, new )
import Foreign.Marshal.Alloc                 ( alloca, free )
import Foreign.Marshal.Array                 ( withArrayLen, newArray )
import Foreign.Marshal.Unsafe                ( unsafeLocalState )
import Foreign.Ptr                           ( freeHaskellFunPtr, Ptr, FunPtr )

import Control.Monad                         ( void, replicateM, forM )
import Data.List                             ( intercalate,partition )
import Data.Traversable                      ( for )
import System.Random                         ( randomIO )

import Language.Rust.Data.Position as P (Spanned(..), Span(..), Position(..), spanOf)
import Language.Fortran.Syntax (Ty(..), Token(..))
--import Data.Maybe
import Data.Int (Int16)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Foreign (peek)

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old n = intercalate n . splitOn old

-- $overview
--
-- This module provides the facility for dropping in bits of Rust code into your
-- Haskell project.
--
-- ** How it works
--
-- This works by the magic of Template Haskell. In a nutshell, for every Haskell
-- source with a Rust quasiquote in it, a Rust source file is generated. Into
-- this file are added
--
--   - all top-level Rust quasiquotes (contents are added in as-is)
--
--   - functions for all expression-level quasiquotes (function arguments
--     correspond to referenced Haskell variables)
--
-- On the Haskell side, every expression quasiquote generates an FFI import
-- to match the generated Rust function and is then replaced with an expression
-- calling that function (passed in as arguments the Haskell variables the
-- quasiquote used).
--
-- The Rust source file is compiled (by `rustc` if there are no extern crates
-- or by `cargo` - dependencies are placed in `.inline-rust-quasi` - if there
-- are). Finally, the resulting static library is passed to GHC through
-- Template Haskell.

-- $quasiquoters
--
-- Rust is (like Haskell) an expression based language, so it is sufficient to
-- make quasiquoters for expressions only. Note that `{ <stmt>; ... }` is a
-- valid Rust block expression.
--
-- As Rust does not distinguish between pure and impure expressions, it is
-- entirely up to the user of this library to use the correct quasiquoter.
-- Quasiquoters with `IO` are meant for impure expressions and the rest are for
-- pure expressions. Incorrectly annotating an impure expression as pure will
-- /not/ cause a compile-time error but may break type safety and referential
-- transparency.

-- $safe
--
-- Safe quasiquoters are the most simple ones. When in doubt and not overly in
-- need of performance, use these.

-- | Safe and pure expression quasiquoter. It is up the user to make sure the
-- Rust expression they use is pure.
--
-- This can also be used in a declaration context to just emit raw Rust code.
-- You can use this to define Rust items that you can use in any quasiquote in
-- the module.
--
-- @
--     rustInc x :: Int32 -> Int32
--     rustInc x = [rust| i32 { 1i32 + $(x: i32) } |]
-- @
rust :: QuasiQuoter
rust = rustQuasiQuoter Safe True True

-- | Safe and impure expression quasiquoter. Like 'rust', this can also be used
-- to emit top-level Rust items when used in declaration context.
--
-- @
--     rustHello :: Int32 -> IO ()
--     rustHello n = [rustIO| () { println!("Your number: {}", $(n: i32)) } |]
-- @
rustIO :: QuasiQuoter
rustIO = rustQuasiQuoter Safe False True

fortIO :: QuasiQuoter
fortIO  = rustQuasiQuoter Safe False True

readOnlyVectors :: (V.Storable a) => [V.Vector a] -> ([Ptr a] -> IO b) -> IO b
readOnlyVectors [] io = io mempty
readOnlyVectors (v:vs) io =
  V.unsafeWith v $ \p -> readOnlyVectors vs $ \sv -> io (p:sv)

unsafeWithVectors :: (V.Storable a) => [VM.IOVector a] -> ([Ptr a] -> IO b) -> IO b
unsafeWithVectors [] io = io mempty
unsafeWithVectors (v:vs) io =
  VM.unsafeWith v $ \p -> unsafeWithVectors vs $ \sv -> io (p:sv)

  {-
withPtrs :: (V.Storable a) => ([Ptr a] -> IO ()) -> IO [a]
withPtrs f = do
  alloca $ \p1 ->
    alloca $ \p2 ->
      alloca $ \p3 -> do
        let ptr = p1:p2:p3:[]
        f ptr
        mapM peek ptr
        -}

withPtrsN :: Int -> Q Exp
withPtrsN n = do
  io <- newName "io"
  xs <- replicateM n (newName "x")
  let lxs  = ListE (map VarE xs)
  [e| \ ($(varP io)) -> $(genAlloca io lxs xs) |]
    where
      genAlloca io lxs []     =
        [e| do
              let p = $(pure lxs)
              $(varE io) p
              mapM peek p
          |]
      genAlloca io lxs (x:xs) = [e| alloca (\( $(varP x) ) -> $(genAlloca io lxs xs) ) |]

genWithPtrs :: Int -> Q [Dec]
genWithPtrs n = fmap concat $ forM [1..n] mkWithPtrs
  where mkWithPtrs i = do
          wP <- withPtrsN i
          wT <- withPtrsNTy
          let name = mkName $ "withPtrs" ++ show i
          return $ [ SigD name wT
                   , FunD name [ Clause [] (NormalB wP) [] ]
                   ]

withPtrsNTy :: TypeQ
withPtrsNTy = do
  let a = mkName "a"
  [t| (VM.Storable $(varT a)) => ([Ptr $(varT a)] -> IO ()) -> IO [$(varT a)] |]

withPtr :: (V.Storable a) => (Ptr a -> IO b) -> IO (a, b)
withPtr f = do
  alloca $ \ptr -> do
    x <- f ptr
    y <- peek ptr
    return (y, x)


-- $unsafe
--
-- Unsafe quasiquoters have less overhead than safe ones, but they can have
-- problems if the Rust expression calls back into the Haskell runtime or
-- if the Rust expression blocks indefinitely.
--
-- This [wiki page](wiki.haskell.org/Foreign_Function_Interface#Unsafe_calls)
-- and the [Haskell Report](www.haskell.org/definition/haskell2010.pdf) section
-- on "Import Declarations" detail the caveats of `unsafe`.

-- | Unsafe but pure expression quasiquoter. It is up the user to make sure the
-- Rust expression they use is pure, doesn't block, and doesn't call
-- back into the Haskell runtime.
--
-- Faster, but use with caution.
rustUnsafe :: QuasiQuoter
rustUnsafe = rustQuasiQuoter Unsafe True False

-- | Unsafe and impure expression quasiquoter. It is up the user to make sure
-- the Rust expression they use doesn't block and doesn't call back into the
-- Haskell runtime.
--
-- Faster, but use with caution.
rustUnsafeIO :: QuasiQuoter
rustUnsafeIO = rustQuasiQuoter Unsafe False False


-- $interruptible
--
-- Interruptible quasiquoters are slightly stronger (and slower) than safe ones:
-- they additionally try to make the foreign call promptly return when a
-- 'throwTo' is directed at a thread making the call.
--
-- The [GHC Docs](downloads.haskell.org/~ghc/latest/docs/html/users_guide/ffi-chap.html#interruptible-foreign-calls)
-- detail the behaviour of 'interruptible'.

-- | Interrupt and pure expression quasiquoter. It is up the user to make sure the
-- Rust expression they use is pure.
--
-- Slower, but safer around exception-heavy code.
rustInterruptible :: QuasiQuoter
rustInterruptible = rustQuasiQuoter Interruptible True False

-- | Interrupt and impure expression quasiquoter.
--
-- Slower, but safer around exception-heavy code.
rustInterruptibleIO :: QuasiQuoter
rustInterruptibleIO = rustQuasiQuoter Interruptible False False


-- | Make an expression/declaration quasiquoter.
--
-- For expressions, this packages together the work of parsing the quasiquote
-- contents, generating Haskell FFI bindings, generating and compiling Rust
-- source, then linking in the Rust object file.
--
-- For declarations (if supported), this emits raw code.
rustQuasiQuoter :: Safety      -- ^ safety of FFI
                -> Bool        -- ^ purity of FFI
                -> Bool        -- ^ support declarations
                -> QuasiQuoter
rustQuasiQuoter safety isPure supportDecs = QuasiQuoter { quoteExp = expQuoter
                                                        , quotePat = err
                                                        , quoteType = err
                                                        , quoteDec = decQuoter
                                                        }
  where
    who | supportDecs = "expressions and declarations"
        | otherwise   = "expressions"

    err = fail ("(inline-rust): Only " ++ who ++ " can be quasiquoted")

    expQuoter qq = do
      parsed <- parseQQ qq
      processQQ safety isPure parsed

    decQuoter | supportDecs = emitCodeBlock
              | otherwise = err


showTy :: Type -> String
showTy = show . pprParendType

-- | This function sums up the packages. What it does:
--
--    1. Map the Rust type annotations in the quasiquote to their Haskell types.
--
--    2. Generate a Haskell FFI signature for passing all the captured Haskell
--       variables to Rust.
--
--    3. Generate a Rust function compatible with the Haskell FFI, and emit it
--       to a temporary file (the same file is kept for the whole module)
--
--    4. Generate and return a Haskell call to this function, slotting in the
--       right Haskell arguments.
--
deFun :: HType -> Q HType
deFun x = do
  funPtr <- [t| FunPtr |]
  go funPtr x
    where
      go ft c = case c of
                  AppT ft1 t -> if (ft1 == ft) then pure t
                                               else error "deFun: wrong ft"
                  _ -> error "deFun"

processQQ :: Safety -> Bool -> RustQuasiquoteParse -> Q Exp
--processQQ safety isPure (QQParse rustRet rustNamedArgs_FnPtr locVars varsInBody rustBody ) = do
processQQ safety isPure (QQParse rustRet rustNamedArgs_FnPtr _ varsInBody rustBody ) = do

  -- Make a name to thread through Haskell/Rust (see Trac #13054)
  q <- runIO randomIO :: Q Int16
  qqName <- newName $ "qq" ++ show (abs q)
  let qqStrName = show qqName

  --let (fortFnPtrNamedArgs,rustNamedArgs) = partition (\(_,(b,_)) -> isFunctionPtr b) rustNamedArgs_FnPtr
  let (fortFnPtrNamedArgs,rustNamedArgs) = partition (\(_,(b,_)) -> isFunctionPtr b) rustNamedArgs_FnPtr
  -- Find out what the corresponding Haskell representations are for the
  -- argument and return types
  runIO $ putStrLn $ "fortFnPtrNamedArgs: " ++ show fortFnPtrNamedArgs
  runIO $ putStrLn $ "rustNamedArgs_FnPtr: " ++ (show $ length rustNamedArgs_FnPtr ) ++ " :: " ++ show rustNamedArgs_FnPtr
  let (retNamedArgs,rustNamedArgs') = partition (\(a,_) -> a == "return__") rustNamedArgs
  let (rustArgNames, rustArgs_intents) = unzip rustNamedArgs'
  let (rustArgs, intents) = unzip rustArgs_intents
  (haskRet, reprCRet) <- getRType (void rustRet)
    {-
  reprCRet <- pure Nothing
  -}
--  haskRet <- [t| () |]  -- this means haskRet will be always void in C
--  runIO $ putStrLn $ "rustArgs:" ++ show rustArgs
--  runIO $ putStrLn $ "intents:" ++ show intents
  runIO $ putStrLn $ "rustRet: " ++ show rustRet
  runIO $ putStrLn $ "reprCRet: " ++ show reprCRet
  (haskArgs, reprCArgs) <- unzip <$> traverse (getRType . void) rustArgs

  -- Convert the Haskell return type to a marshallable FFI type
  (procedure,returnFfi, haskRet') <- do
    marshalFrom <- ghcMarshallable haskRet
    runIO $ putStrLn $ "marshalFrom: " ++ show marshalFrom ++ "_" ++ showTy haskRet
    -- we will always have ret of BoxedDirect
    ret <- case marshalFrom of
             BoxedDirect -> [t| IO $(pure haskRet) |]
             BoxedIndirect -> [t| Ptr $(pure haskRet) -> IO () |]
             UnboxedDirect
               | isPure -> pure haskRet
               | otherwise ->
                   let retTy = showTy haskRet
                   in fail ("Cannot put unlifted type ‘" ++ retTy ++ "’ in IO")
    let retTy = showTy haskRet
    procedure  <- pure $ if retTy == "()" then Subroutine
                                          else Function
    pure (procedure,marshalFrom, pure ret)

  -- Convert the Haskell arguments to marshallable FFI types
  (argsByVal, haskArgs') <- fmap unzip $
    for (zip haskArgs intents) $ \(haskArg,intent) -> do
      marshalForm <- ghcMarshallable haskArg
      runIO $ putStrLn $ "marshalForm: " ++ show marshalForm
       -- cause everything is passed as pointer
      ptr <- case intent of
               "value" -> [t| $(pure haskArg) |]
               _ -> [t| Ptr $(pure haskArg) |]
      pure (True, ptr)

  --haskArgsFunPtr <- do
  let (_, rustArgs_intentsFunPtr) = unzip fortFnPtrNamedArgs
  let (rustArgsFunPtr, _) = unzip rustArgs_intentsFunPtr
  (haskArgsFunPtr, _) <- unzip <$> traverse (getRType . void) rustArgsFunPtr
  -- Generate the Haskell FFI import declaration and emit it
  haskSig <- foldr (\l r -> [t| $(pure l) -> $r |]) haskRet' $ haskArgs' ++ haskArgsFunPtr
  --fail $
  runIO $ putStrLn $
    intercalate " ===:===\n "
      [ show haskArgsFunPtr
      , show haskSig
      ]
  let ffiImport = ForeignD (ImportF CCall safety (qqStrName ++ "_") qqName haskSig)
  addTopDecls [ffiImport]

  -- Generate the Haskell FFI call
  let goArgs :: [Q Exp]           -- ^ arguments accumulated so far (reversed)
             -> [(String, Bool, Ty Span)]  -- ^ remaining arguments to process
             -> Q Exp             -- ^ FFI call

      goArgs acc []
        | returnFfi /= BoxedIndirect = appsE (varE qqName : reverse acc)
        | otherwise = do
            ret <- newName "ret"
            [e| alloca (\( $(varP ret) ) ->
                  do { $(appsE (varE qqName : reverse (varE ret : acc)))
                     ; peek $(varE ret)
                     }) |]

      goArgs acc (("return__",_ ,_) : args) = goArgs acc args
      goArgs acc (a@(argStr,_ ,rustArg) : args) = do
        runIO $ putStrLn $ "goArgs: args: " ++ show a
        arg <- lookupValueName argStr
        case arg of
          Nothing -> fail ("Could not find Haskell variable ‘" ++ argStr ++ "’")
          Just argName -> do
            withVar goArgs argName acc args (HaskVar rustArg)
  let haskCall' = goArgs [] (zip3 rustArgNames argsByVal rustArgs)
      haskCall1 :: Q Exp
      haskCall1 = if isPure && returnFfi /= UnboxedDirect
                   then [e| unsafeLocalState $haskCall' |]
                   else haskCall'
  haskCall <- case fortFnPtrNamedArgs of
    [] -> return haskCall1
    _ -> do
      ptr <- newName "ptr" :: Q Name
      ret <- newName "ret" :: Q Name
      let
          (haskArgsFunPtr1:_) = haskArgsFunPtr :: [HType]
          (rustArgsFunPtr1:_) = rustArgsFunPtr
          theF = takeFunctionName rustArgsFunPtr1 :: String
      let haskArgsFunPtr2 = deFun haskArgsFunPtr1
      theF1 <- lookupValueName $ takeFunctionName rustArgsFunPtr1
      let theFun :: Name
          theFun = case theF1 of
                  Just f -> f
                  _ -> error $ "haskCall: lookupValueName: can not find function: " ++ theF
      let
          haskCall2 :: Q Exp
          haskCall2 =
            [e| do { $(varP ptr) <- $(newFunPtr haskArgsFunPtr2) $(varE theFun)
                   ; $(varP ret) <- (\a f -> f a) $(varE ptr) $ $(haskCall1)
                   ; freeHaskellFunPtr $(varE ptr)
                   ; pure $(varE ret)
                   }
            |]
      ttt <- haskCall2
      --fail
      runIO $ putStrLn
        $ "haskCall :: "  ++ show ttt -- (show haskArgsFunPtr2)  -- theFun ++ show theF -- rustArgsFunPtr1 -- haskArgsFunPtr1 --  ttt
      return haskCall2
  -- Generate the Rust function arguments and the converted arguments
  let (rustArgs', _) = unzip $ zipWith mergeArgs rustArgs reprCArgs

     -- mergeArgs :: Ty Span -> Maybe RType -> (Ty Span, Ty Span)
      mergeArgs t Nothing       = (t, t)
      mergeArgs t (Just tInter) = (fmap (const mempty) tInter, t)

  -- Generate the Rust function.
  let fortFnPtrNames = map ((++ "_cptr") . takeFnName1) fortFnPtrNamedArgs
  let (headSubroutine',endProcedure) =
        let param = "(" ++ intercalate ", " (fortFnPtrNames ++ rustArgNames) ++ ")"
         in case procedure of
          Subroutine -> ( "      subroutine " ++ qqStrName ++ param
                        , "      end subroutine " )
          Function -> ("      function " ++ qqStrName ++ param
                          , "      end function  ")
  let (h1:h1s) = chunksOf 60 headSubroutine'
  let headSubroutine = unlines $ h1:(map ("     c" ++) h1s)
  let retVarStatement = case retNamedArgs of
                          [] -> ""
                          ((_,(retTy,_)):_) -> "      "++ renderType retTy ++ " :: " ++ qqStrName
  void . emitCodeBlock . unlines $
    [ headSubroutine
    , "      USE, INTRINSIC :: ISO_C_BINDING"
    , unlines $ map renderFortran varsInBody
    , "cccccccccccccc VarStatements"
    , unlines $ map renderVarStatement $ zip3 rustArgNames rustArgs' $ zip intents rustArgs
    , retVarStatement
    , renderFuncInterface fortFnPtrNamedArgs
--  , unlines $ map renderFortran $ drop (fromMaybe 0 locVars) varsInBody
    , "cccccccccccccc bodyStatements"
    , replace "return__" qqStrName $ unlines $ map renderFortran rustBody
    , endProcedure ++ qqStrName
    ]

  -- Return the Haskell call to the FFI import
  haskCall
    where
      genHeadSubroutine hs =
        let (h1:h1s) = chunksOf 60 hs
         in unlines $ h1:(map ("     c" ++) h1s)

      takeFnName1 (_,(FProcedurePtr fn _ _ _, _)) = fn
      takeFnName1 _ = error "takeFnName1: wrong pattern"

      takeFunctionName (FProcedurePtr n _ _ _) = n
      takeFunctionName _ = error "takeFunctionName: wrong pattern"

      genFuncPtrAssign (_,(FProcedurePtr fn _ _ _, _)) =
        "      call c_f_procpointer("++ fn ++ "_cptr," ++ fn ++"_fptr)"
      genFuncPtrAssign _ = error "genFuncPtrAssign: wrong pattern"

      genFuncPtrFort (_,(FProcedurePtr fn  _ _ _, _)) =
        "      procedure(" ++ fn ++ "), pointer :: " ++ fn ++ "_fptr"
      genFuncPtrFort _ = error "genFuncPtrFort: undefined input"

      genFuncPointer (_,(FProcedurePtr fn _ _ _, _)) =
        "      type(C_FUNPTR),intent(in),value :: " ++ fn ++ "_cptr"
      genFuncPointer _ = error "genFuncPointer: undefined input"

      renderVarType (v,t) = renderType t ++ ",intent(in),value :: " ++ v

      genFuncInterface (_,(FProcedurePtr fn retTy paramTys _, _)) =
        let paramVars = (map ((fn ++) . show) $ [1..(length paramTys)])
            paramList = "(" ++ (intercalate "," paramVars ) ++ ")"
            funcStatement = "function " ++ (unwords [ fn , paramList ])
            (h:rs) = map ("      " ++) $
              [ funcStatement
              , unlines $ map renderVarType $ zip paramVars paramTys
              , renderType retTy ++ " :: " ++ fn
              , "end function " ++ fn
              ]
         in unlines $ (genHeadSubroutine h):rs
      genFuncInterface _ = error "genFuncInterface: wrong type of input, FProcedurePtr needed"

      renderFuncInterface [] = ""
      renderFuncInterface xs = unlines
        [  "      interface"
        , concat $ map genFuncInterface xs
        , "      end interface"
        , unlines $ map genFuncPointer xs
        , unlines $ map genFuncPtrFort xs
        , unlines $ map genFuncPtrAssign xs
        ]

      renderVarStatement (s,(FString _),(_,_)) = "c     " ++ s ++ " needs manual declaration for its length"
      renderVarStatement (s,(FArray d t _),(i,_)) =
        let intent = "intent(" ++ i ++ ")"
            dim = case d of
                    0 -> ""
                    _ -> "(" ++ ( intercalate "," $ replicate d ":") ++ ")"
         in "      " ++ (renderType t) ++ "," ++ intent ++  " :: " ++ s ++ dim
      renderVarStatement (s,t,(i,r)) =
        let intent = case i of
                       "in" -> "intent(" ++ i ++ ")"
                       "out" -> "intent(" ++ i ++ ")"
                       "inout" -> "intent(" ++ i ++ ")"
                       _ -> i
            (_,dim) = case r of
                         (Array _ st _) -> (t, ",dimension" ++ renderExpr st)
                         _ -> (r,"")
         in "      " ++ (renderType t) ++ "," ++ intent ++ dim ++ " :: " ++ s

      withVar f argName acc args (HaskVar _) = do
              f (varE argName : acc) args
      pad6Blanks p = take (p-1) "      "
      renderFortran [] = ""
      renderFortran tok@(tt@(Spanned t _):_) =
        let pn = case (lo $ spanOf tt) of
                   NoPosition -> 7
                   Position _ _ a -> a
            ad = case t of
                  TNewLine  -> ""
                  Ampersand -> "     "
                  _         -> if (7 >= pn) then (pad6Blanks pn)
                                            else "      "
         in ad ++ (takeWhile (/= '\n') $ renderTokens tok)

isFunctionPtr :: Ty Span -> Bool
isFunctionPtr (FProcedurePtr _ _ _ _) = True
isFunctionPtr _ = False

newtype HaskVar a = HaskVar a
