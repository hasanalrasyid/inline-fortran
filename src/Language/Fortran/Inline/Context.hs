{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | A 'Context' is used to define the capabilities of the Template Haskell code
-- that handles the inline C code. See the documentation of the data type for
-- more details.
--
-- In practice, a 'Context' will have to be defined for each library that
-- defines new C types, to allow the TemplateHaskell code to interpret said
-- types correctly.

module Language.Fortran.Inline.Context
  ( -- * 'TypesTable'
    TypesTable
  , convertType
  , CArray
  , isTypeName

    -- * 'AntiQuoter'
  , AntiQuoter(..)
  , AntiQuoterId
  , SomeAntiQuoter(..)
  , AntiQuoters

    -- * 'Context'
  , Context(..)
  , baseCtx
  , funCtx
  , vecCtx
  , VecCtx(..)
  , bsCtx
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad (mzero)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Typeable (Typeable)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Foreign.Ptr (Ptr, FunPtr, castPtr)
import           Foreign.Storable (Storable)
import qualified Language.Haskell.TH as TH
import qualified Text.Parser.Token as Parser

#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
#endif

import           Language.Fortran.Inline.FunPtr
import qualified Language.Fortran.Types as C

-- | A mapping from 'C.TypeSpecifier's to Haskell types.  Needed both to
-- parse C types, and to convert them to Haskell types.
type TypesTable = Map.Map C.TypeSpecifier TH.TypeQ

-- | Specifies how to parse and process an antiquotation in the C code.
--
-- All antiquotations (apart from plain variable capture) have syntax
--
-- @
-- $XXX:YYY
-- @
--
-- Where @XXX@ is the name of the antiquoter and @YYY@ is something
-- parseable by the respective 'aqParser'.
data AntiQuoter a = AntiQuoter
  { aqParser :: forall m. C.CParser m => m (String, C.Type, a)
    -- ^ Parses the body of the antiquotation, returning an hint for the name to
    -- assign to the variable that will replace the anti-quotation, the type of
    -- said variable, and some arbitrary data which will then be fed to
    -- 'aqMarshaller'.
  , aqMarshaller :: TypesTable -> C.Type -> a -> TH.Q (TH.Type, TH.Exp)
    -- ^ Takes the type and the body returned by 'aqParser', together with the
    -- current 'TypesTable'.
    --
    -- Returns the Haskell type for the parameter, and the Haskell expression
    -- that will be passed in as the parameter.
    --
    -- If the the type returned is @ty@, the 'TH.Exp' __must__ have type @forall
    -- a. (ty -> IO a) -> IO a@. This allows to do resource handling when
    -- preparing C values.
  }

-- | An identifier for a 'AntiQuoter'.
type AntiQuoterId = String

-- | Existential wrapper around 'AntiQuoter'.
data SomeAntiQuoter = forall a. (Eq a, Typeable a) => SomeAntiQuoter (AntiQuoter a)

type AntiQuoters = Map.Map AntiQuoterId SomeAntiQuoter

-- | A 'Context' stores various information needed to produce the files with
-- the C code derived from the inline C snippets.
--
-- 'Context's can be composed with their 'Monoid' instance, where 'mappend' is
-- right-biased -- in @'mappend' x y@ @y@ will take precedence over @x@.
data Context = Context
  { ctxTypesTable :: TypesTable
    -- ^ Needed to convert C types to Haskell types.
  , ctxAntiQuoters :: AntiQuoters
    -- ^ Needed to parse and process antiquotations.
  , ctxFileExtension :: Maybe String
    -- ^ Will determine the extension of the file containing the inline
    -- C snippets.
  , ctxOutput :: Maybe (String -> String)
    -- ^ This function is used to post-process the functions generated
    -- from the C snippets.  Currently just used to specify C linkage
    -- when generating C++ code.
  }

instance Monoid Context where
  mempty = Context
    { ctxTypesTable = mempty
    , ctxAntiQuoters = mempty
    , ctxFileExtension = Nothing
    , ctxOutput = Nothing
    }

  mappend ctx2 ctx1 = Context
    { ctxTypesTable = ctxTypesTable ctx1 <> ctxTypesTable ctx2
    , ctxAntiQuoters = ctxAntiQuoters ctx1 <> ctxAntiQuoters ctx2
    , ctxFileExtension = ctxFileExtension ctx1 <|> ctxFileExtension ctx2
    , ctxOutput = ctxOutput ctx1 <|> ctxOutput ctx2
    }

-- | Context useful to work with vanilla C. Used by default.
--
-- 'ctxTypesTable': converts C basic types to their counterparts in
-- "Foreign.C.Types".
--
-- No 'ctxAntiQuoters'.
baseCtx :: Context
baseCtx = mempty
  { ctxTypesTable = baseTypesTable
  }

baseTypesTable :: Map.Map C.TypeSpecifier TH.TypeQ
baseTypesTable = Map.fromList
  [ (C.Void, [t| () |])
  , (C.Char Nothing, [t| CChar |])
  , (C.Char (Just C.Signed), [t| CChar |])
  , (C.Char (Just C.Unsigned), [t| CUChar |])
  , (C.Short C.Signed, [t| CShort |])
  , (C.Short C.Unsigned, [t| CUShort |])
  , (C.Int C.Signed, [t| CInt |])
  , (C.Int C.Unsigned, [t| CUInt |])
  , (C.Long C.Signed, [t| CLong |])
  , (C.Long C.Unsigned, [t| CULong |])
  , (C.LLong C.Signed, [t| CLLong |])
  , (C.LLong C.Unsigned, [t| CULLong |])
  , (C.Float, [t| CFloat |])
  , (C.FloatFORTRANX, [t| CFloat |])
  , (C.Double, [t| CDouble |])
  ]

-- | An alias for 'Ptr'.
type CArray = Ptr

------------------------------------------------------------------------
-- Type conversion

-- | Given a 'Context', it uses its 'ctxTypesTable' to convert
-- arbitrary C types.
convertType
  :: TypesTable
  -> C.Type
  -> TH.Q (Maybe TH.Type)
convertType cTypes = runMaybeT . go
  where
    goDecl = go . C.parameterDeclarationType

    go :: C.Type -> MaybeT TH.Q TH.Type
    go cTy = case cTy of
      C.TypeSpecifier _specs cSpec ->
        case Map.lookup cSpec cTypes of
          Nothing -> mzero
          Just ty -> lift ty
      C.Ptr _quals (C.Proto retType pars) -> do
        hsRetType <- go retType
        hsPars <- mapM goDecl pars
        lift [t| FunPtr $(buildArr hsPars hsRetType) |]
      C.Ptr _quals cTy' -> do
        hsTy <- go cTy'
        lift [t| Ptr $(return hsTy) |]
      C.Array _mbSize cTy' -> do
        hsTy <- go cTy'
        lift [t| CArray $(return hsTy) |]
      C.Proto _retType _pars -> do
        -- We cannot convert standalone prototypes
        mzero

    buildArr [] hsRetType =
      [t| IO $(return hsRetType) |]
    buildArr (hsPar : hsPars) hsRetType =
      [t| $(return hsPar) -> $(buildArr hsPars hsRetType) |]

isTypeName :: TypesTable -> C.Identifier -> Bool
isTypeName cTypes id' = Map.member (C.TypeName id') cTypes

------------------------------------------------------------------------
-- Useful contexts

getHsVariable :: String -> String -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName s
  case mbHsName of
    Nothing -> error $ "Cannot capture Haskell variable " ++ s ++
                       ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

convertType_ :: String -> TypesTable -> C.Type -> TH.Q TH.Type
convertType_ err cTypes cTy = do
  mbHsType <- convertType cTypes cTy
  case mbHsType of
    Nothing -> error $ "Cannot convert C type (" ++ err ++ ")"
    Just hsType -> return hsType

-- | This 'Context' includes a 'AntiQuoter' that removes the need for
-- explicitely creating 'FunPtr's, named @"fun"@.
--
-- For example, we can capture function @f@ of type @CInt -> CInt -> IO
-- CInt@ in C code using @$fun:(int (*f)(int, int))@.
--
-- Does not include the 'baseCtx', since most of the time it's going to
-- be included as part of larger contexts.
funCtx :: Context
funCtx = mempty
  { ctxAntiQuoters = Map.fromList [("fun", SomeAntiQuoter funPtrAntiQuoter)]
  }

funPtrAntiQuoter :: AntiQuoter String
funPtrAntiQuoter = AntiQuoter
  { aqParser = do
      cTy <- Parser.parens C.parseParameterDeclaration
      case C.parameterDeclarationId cTy of
        Nothing -> error "Every captured function must be named (funCtx)"
        Just id' -> do
         let s = C.unIdentifier id'
         return (s, C.parameterDeclarationType cTy, s)
  , aqMarshaller = \cTypes cTy cId -> do
      hsTy <- convertType_ "funCtx" cTypes cTy
      hsExp <- getHsVariable "funCtx" cId
      case hsTy of
        TH.AppT (TH.ConT n) hsTy' | n == ''FunPtr -> do
          hsExp' <- [| \cont -> cont =<< $(mkFunPtr (return hsTy')) $(return hsExp) |]
          return (hsTy, hsExp')
        _ -> error "The `fun' marshaller captures function pointers only"
  }

-- | This 'Context' includes two 'AntiQuoter's that allow to easily use
-- Haskell vectors in C.
--
-- Specifically, the @vec-len@ and @vec-ptr@ will get the length and the
-- pointer underlying mutable ('V.IOVector') and immutable ('V.Vector')
-- storable vectors.
--
-- Note that if you use 'vecCtx' to manipulate immutable vectors you
-- must make sure that the vector is not modified in the C code.
--
-- To use @vec-len@, simply write @$vec-len:x@, where @x@ is something
-- of type @'V.IOVector' a@ or @'V.Vector' a@, for some @a@.  To use
-- @vec-ptr@ you need to specify the type of the pointer,
-- e.g. @$vec-len:(int *x)@ will work if @x@ has type @'V.IOVector'
-- 'CInt'@.
vecCtx :: Context
vecCtx = mempty
  { ctxAntiQuoters = Map.fromList
      [ ("vec-ptr", SomeAntiQuoter vecPtrAntiQuoter)
      , ("vec-len", SomeAntiQuoter vecLenAntiQuoter)
      ]
  }

-- | Type class used to implement the anti-quoters in 'vecCtx'.
class VecCtx a where
  type VecCtxScalar a :: *

  vecCtxLength :: a -> Int
  vecCtxUnsafeWith :: a -> (Ptr (VecCtxScalar a) -> IO b) -> IO b

instance Storable a => VecCtx (V.Vector a) where
  type VecCtxScalar (V.Vector a) = a

  vecCtxLength = V.length
  vecCtxUnsafeWith = V.unsafeWith

instance Storable a => VecCtx (VM.IOVector a) where
  type VecCtxScalar (VM.IOVector a) = a

  vecCtxLength = VM.length
  vecCtxUnsafeWith = VM.unsafeWith

vecPtrAntiQuoter :: AntiQuoter String
vecPtrAntiQuoter = AntiQuoter
  { aqParser = do
      cTy <- Parser.parens C.parseParameterDeclaration
      case C.parameterDeclarationId cTy of
        Nothing -> error "Every captured vector must be named (vecCtx)"
        Just id' -> do
         let s = C.unIdentifier id'
         return (s, C.parameterDeclarationType cTy, s)
  , aqMarshaller = \cTypes cTy cId -> do
      hsTy <- convertType_ "vecCtx" cTypes cTy
      hsExp <- getHsVariable "vecCtx" cId
      hsExp' <- [| vecCtxUnsafeWith $(return hsExp) |]
      return (hsTy, hsExp')
  }

vecLenAntiQuoter :: AntiQuoter String
vecLenAntiQuoter = AntiQuoter
  { aqParser = do
      cId <- C.parseIdentifier
      let s = C.unIdentifier cId
      return (s, C.TypeSpecifier mempty (C.Long C.Signed), s)
  , aqMarshaller = \_cTypes cTy cId -> do
      case cTy of
        C.TypeSpecifier _ (C.Long C.Signed) -> do
          hsExp <- getHsVariable "vecCtx" cId
          hsExp' <- [| fromIntegral (vecCtxLength $(return hsExp)) |]
          hsTy <- [t| CLong |]
          hsExp'' <- [| \cont -> cont $(return hsExp') |]
          return (hsTy, hsExp'')
        _ -> do
          error "impossible: got type different from `long' (vecCtx)"
  }


-- | 'bsCtx' serves exactly the same purpose as 'vecCtx', but only for
-- 'BS.ByteString'.  @vec-ptr@ becomes @bs-ptr@, and @vec-len@ becomes
-- @bs-len@.  You don't need to specify the type of the pointer in
-- @bs-ptr@, it will always be @unsigned char*@.
bsCtx :: Context
bsCtx = mempty
  { ctxAntiQuoters = Map.fromList
      [ ("bs-ptr", SomeAntiQuoter bsPtrAntiQuoter)
      , ("bs-len", SomeAntiQuoter bsLenAntiQuoter)
      ]
  }

bsPtrAntiQuoter :: AntiQuoter String
bsPtrAntiQuoter = AntiQuoter
  { aqParser = do
      cId <- C.parseIdentifier
      let s = C.unIdentifier cId
      return (s, C.Ptr [] (C.TypeSpecifier mempty (C.Char (Just C.Unsigned))), s)
  , aqMarshaller = \_cTypes cTy cId -> do
      case cTy of
        C.Ptr _ (C.TypeSpecifier _ (C.Char (Just C.Unsigned))) -> do
          hsTy <- [t| Ptr CUChar |]
          hsExp <- getHsVariable "bsCtx" cId
          hsExp' <- [| \cont -> BS.unsafeUseAsCString $(return hsExp) $ \ptr -> cont (castPtr ptr)  |]
          return (hsTy, hsExp')
        _ ->
          error "impossible: got type different from `unsigned char' (bsCtx)"
  }

bsLenAntiQuoter :: AntiQuoter String
bsLenAntiQuoter = AntiQuoter
  { aqParser = do
      cId <- C.parseIdentifier
      let s = C.unIdentifier cId
      return (s, C.TypeSpecifier mempty (C.Long C.Signed), s)
  , aqMarshaller = \_cTypes cTy cId -> do
      case cTy of
        C.TypeSpecifier _ (C.Long C.Signed) -> do
          hsExp <- getHsVariable "bsCtx" cId
          hsExp' <- [| fromIntegral (BS.length $(return hsExp)) |]
          hsTy <- [t| CLong |]
          hsExp'' <- [| \cont -> cont $(return hsExp') |]
          return (hsTy, hsExp'')
        _ -> do
          error "impossible: got type different from `long' (bsCtx)"
  }
