{-|
Module      : Language.Rust.Inline.Context
Description : Defines contexts (rules mapping Rust types to Haskell types)
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}

module Language.Fortran.Inline.Context where

import Language.Fortran.Inline.Pretty ( renderType )

import Language.Fortran.Quote         ( ty )
import Language.Fortran.Syntax        ( Ty(..), Abi(..), FnDecl(..),
                                     Arg(..), Mutability(..), Unsafety(..) )

import Language.Haskell.TH

--import Data.Semigroup             hiding (Arg)
import Data.Monoid
import Data.Typeable               ( Typeable )
import Control.Monad               ( void, liftM2, fail )
import Data.Traversable            ( for )
import Data.List                   ( intercalate )

import Data.Int                    ( Int8, Int16, Int32, Int64 )
import Data.Word                   ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr                 ( Ptr, FunPtr )
import Foreign.C.Types             -- pretty much every type here is used

import GHC.Exts                    ( Char#, Int#, Word#, Float#, Double#,
--                                   ByteArray#
                                   )

import qualified Control.Monad.Fail as Fail
import Eigen.Internal -- CComplex
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable as V
--import qualified Debug.Trace as D

import Language.Rust.Data.Position
import Foreign hiding (void)

import Data.Complex

instance Fail.MonadFail First where
  fail x = error $ "MonadFail First error: " ++ x

--start for Vector Context
type CArray = Ptr

-- Easier on the eyes
type RType = Ty ()
type HType = Type

-- | Represents a prioritized set of rules for mapping Haskell types into Rust
-- ones and vice versa.
--
-- The 'Context' argument encodes the fact that we may need look
-- recursively into the 'Context' again before possibly producing a Haskell
-- type.
newtype Context =
    Context ( [ RType -> Context -> First (Q HType, Maybe (Q RType),String) ]
            -- Given a Rust type in a quasiquote, we need to look up the
            -- corresponding Haskell type (for the FFI import) as well as the
            -- C-compatible Rust type (if the initial Rust type isn't already
            -- @#[repr(C)]@.

            , [ HType -> Context -> First (Q RType) ]
            -- Given a field in a Haskell ADT, we need to figure out which
            -- (not-necessarily @#[repr(C)]@) Rust type normally maps into this
            -- Haskell type.

            , [ String ]
            -- Source for the trait impls of @MarshalTo@
            , String
            )
  deriving (Semigroup, Monoid, Typeable)

-- | Applicative lifting of the 'Context' instance
instance Semigroup (Q Context) where
  (<>) = liftM2 (<>)

-- | Applicative lifting of the 'Context' instance
instance Monoid (Q Context) where
  mappend = (<>)
  mempty = pure mempty


-- | Search in a 'Context' for the Haskell type corresponding to a Rust type.
-- If the Rust type is not C-compatible, also return a C compatible type. It is
-- expected that:
--
--   1. The Haskell type have a 'Storable' instance
--   2. The C-compatible Rust type have the same layout
--
lookupRTypeInContext :: RType -> Context -> First (Q HType, Maybe (Q RType),String)
lookupRTypeInContext rustType context@(Context (rules, _, _,_)) =
  foldMap (\fits -> fits rustType context) rules

isFProcedurePtr :: RType -> Bool
isFProcedurePtr (FProcedurePtr _ _ _ _) = True
isFProcedurePtr _ = False

-- | Search in a 'Context' for the Rust type corresponding to a Haskell type.
-- Looking up the Rust type using 'lookupRTypeInContext' should yield the
-- initial Haskell type again.
lookupHTypeInContext :: HType -> Context -> First (Q RType)
lookupHTypeInContext haskType context@(Context (_, rules, _,_)) =
  foldMap (\fits -> fits haskType context) rules

-- | Partial version of 'lookupRTypeInContext' that fails with an error message
-- if the type is not convertible.
getRTypeInContext :: RType -> Context -> (Q HType, Maybe (Q RType),String)
getRTypeInContext rustType context =
  let x = takeIdContext context
   in case getFirst (lookupRTypeInContext rustType context) of
    Just found -> found
    Nothing -> ( fail $ unwords [ "Could not find information about"
                                , renderType rustType
                                , "in the context " ++ x ++ " : " ++ show rustType
                                ]
               , Nothing, "failed" )

takeIdContext :: Context -> String
takeIdContext (Context (_,_,_,i)) = i

-- | Partial version of 'lookupHTypeInContext' that fails with an error message
-- if the type is not convertible.
getHTypeInContext :: HType -> Context -> Q RType
getHTypeInContext haskType context =
  case getFirst (lookupHTypeInContext haskType context) of
    Just found -> found
    Nothing -> fail $ unwords [ "Could not find information about"
                              , pprint haskType
                              , "in the context"
                              ]

-- | Make a 'Context' consisting of rules to map the Rust types on the left to
-- the Haskell types on the right. The Rust types should all be @#[repr(C)]@
-- and the Haskell types should all be 'Storable'.
mkContext :: String -> [(Ty a, Q HType, Bool,String)] -> Q Context
mkContext idContext tys = do
    tys' <- traverse (\(rt,qht,mkImpl,idSubcon) -> do { ht <- qht; pure (void rt,ht,mkImpl,idSubcon) }) tys
    pure (Context ( map fits tys'
                  , map rev tys'
                  , map impl tys'
                  , idContext
                  ))
  where
    fits (rts, hts, _, i) rt _ | rt == rts = pure (pure hts, Nothing,i)
                            | otherwise = mempty

    rev (rts, hts, _,_) ht _  | ht == hts = pure (pure rts)
                            | otherwise = mempty


    impl (rts, _, mkImpl,_)   | mkImpl = implMarshalInto rts
                            | otherwise = mempty


-- | Make a default @MarshalInto@ trait impl. (An identity impl)
implMarshalInto :: Ty () -> String
implMarshalInto t = unlines [ "impl MarshalInto<" ++ tyStr ++ "> for " ++ tyStr ++ " {"
                             , "  fn marshal(self) -> " ++ tyStr ++ " { self }"
                             , "}"
                             ]
  where tyStr = renderType t

-- | Make a singleton 'Context' consisting of a rule to map the given Rust type
-- to the given Haskell type.
singleton :: Ty a -> Q HType -> Q Context
singleton rts qht = mkContext "singleton" [(rts, qht, True,"singleton")]


-- * Some handy contexts

-- | Types defined in 'Foreign.C.Types' and the 'libc' crate.
--
-- There should be no conversion required here - these have /identical/ memory
-- layouts (since they both promise to have the same memory layout as C) and are
-- passed on the stack.
libc :: Q Context
libc = mkContext "libc" []
--h--  [ ([ty| libc::c_char      |], [t| CChar      |], False) -- char
--h--  , ([ty| libc::c_schar     |], [t| CSChar     |], False) -- signed char
--h--  , ([ty| libc::c_uchar     |], [t| CUChar     |], False) -- unsigned char
--h--  , ([ty| libc::c_short     |], [t| CShort     |], False) -- short
--h--  , ([ty| libc::c_ushort    |], [t| CUShort    |], False) -- unsigned short
--h--  , ([ty| libc::c_int       |], [t| CInt       |], False) -- int
--h--  , ([ty| libc::c_uint      |], [t| CUInt      |], False) -- unsigned int
--h--  , ([ty| libc::c_long      |], [t| CLong      |], False) -- long
--h--  , ([ty| libc::c_ulong     |], [t| CULong     |], False) -- unsigned long
--h--  , ([ty| libc::ptrdiff_t   |], [t| CPtrdiff   |], False) -- ptrdiff_t
--h--  , ([ty| libc::size_t      |], [t| CSize      |], False) -- size_t
--h--  , ([ty| libc::wchar_t     |], [t| CWchar     |], False) -- wchar_t
--h--  , ([ty| libc::c_longlong  |], [t| CLLong     |], False) -- long long
--h--  , ([ty| libc::c_ulonglong |], [t| CULLong    |], False) -- unsigned long long
--h--  , ([ty| libc::boolean_t   |], [t| CBool      |], False) -- bool
--h--  , ([ty| libc::intptr_t    |], [t| CIntPtr    |], False) -- intptr_t
--h--  , ([ty| libc::uintptr_t   |], [t| CUIntPtr   |], False) -- uintptr_t
--h--  , ([ty| libc::intmax_t    |], [t| CIntMax    |], False) -- intmax_t
--h--  , ([ty| libc::uintmax_t   |], [t| CUIntMax   |], False) -- unsigned intmax_t
--h--  , ([ty| libc::clock_t     |], [t| CClock     |], False) -- clock_t
--h--  , ([ty| libc::time_t      |], [t| CTime      |], False) -- time_t
--h--  , ([ty| libc::useconds_t  |], [t| CUSeconds  |], False) -- useconds_t
--h--  , ([ty| libc::suseconds_t |], [t| CSUSeconds |], False) -- suseconds_t
--h--  , ([ty| libc::c_float     |], [t| CFloat     |], False) -- float
--h--  , ([ty| libc::c_double    |], [t| CDouble    |], False) -- double
--h--  , ([ty| libc::FILE        |], [t| CFile      |], True)  -- FILE
--h--  , ([ty| libc::fpos_t      |], [t| CFpos      |], True)  -- fpos_t
--h--  , ([ty| libc::int8_t      |], [t| Int8       |], False) -- int8_t
--h--  , ([ty| libc::int16_t     |], [t| Int16      |], False) -- int16_t
--h--  , ([ty| libc::int32_t     |], [t| Int32      |], False) -- int32_t
--h--  , ([ty| libc::int64_t     |], [t| Int64      |], False) -- int64_t
--h--  , ([ty| libc::uint8_t     |], [t| Word8      |], False) -- uint8_t
--h--  , ([ty| libc::uint16_t    |], [t| Word16     |], False) -- uint16_t
--h--  , ([ty| libc::uint32_t    |], [t| Word32     |], False) -- uint32_t
--h--  , ([ty| libc::uint64_t    |], [t| Word64     |], False) -- uint64_t
--h--  ]

vecCtx :: Q Context
vecCtx = mkContext "vecCtx"
  [ ([ty| vecptr |], [t| VM.IOVector |], True ,"vecptr")
  , ([ty| veclen |], [t| Int |], True         ,"veclen")
  ]
-- | Basic numeric (and similar) Haskell and Rust types.
--
-- There should be no conversion required here as these should have identical
-- memory layouts.
basic :: Q Context
basic = do
  runIO $ putStrLn $  "=====debug:" ++ show [ty| character |]
  mkContext "basic"
    [ ([ty| char  |], [t| Char    |], True, "char ") -- 4 bytes
    , ([ty| i8    |], [t| Int8    |], True, "i8   ")
    , ([ty| i16   |], [t| Int16   |], True, "i16  ")
    , ([ty| i32   |], [t| Int32   |], True, "i32  ")
    , ([ty| i64   |], [t| Int64   |], True, "i64  ")
    , ([ty| u8    |], [t| Word8   |], True, "u8   ")
    , ([ty| u16   |], [t| Word16  |], True, "u16  ")
    , ([ty| u32   |], [t| Word32  |], True, "u32  ")
    , ([ty| u64   |], [t| Word64  |], True, "u64  ")
    , ([ty| f32   |], [t| Float   |], True, "f32  ")
    , ([ty| f64   |], [t| Double  |], True, "f64  ")
    , ([ty| isize |], [t| Int     |], True, "isize")
    , ([ty| usize |], [t| Word    |], True, "usize")
    , ([ty| bool  |], [t| Word8   |], True, "bool ")
    , ([ty| ()    |], [t| ()      |], True, "()   ")
--   Fortran
    , ([ty| integer       |], [t| Int            |], True , "integer     ")
    , ([ty| logical       |], [t| Int8            |], True , "logical     ")
    , ([ty| real          |], [t| Float           |], True , "real        ")
--  , ([ty| complex       |], [t| CComplex Float  |], True , "complex     ")
    , ([ty| complex(kind=8) |], [t| Ptr (CComplex Double) |], True , "complexDouble ")
    , ([ty| character     |], [t| Char           |], True , "character   ")
    , ([ty| real(kind=8)  |], [t| Double         |], True , "real(kind=8)")
    , ((FString (Span NoPosition NoPosition)), [t|CChar |], True, "FString")
    {-
    , ([ty| logical(kind=1) |], [t| Int8            |])
    , ([ty| character(len=1)|], [t| CChar           |])
--    , ([ty| integer         |], [t| CInt            |])
--    , ([ty| integer(kind=2) |], [t| CShort          |])
--    , ([ty| integer(kind=4) |], [t| CInt            |])
--    , ([ty| integer(kind=4) |], [t| CLong           |])
--    , ([ty| integer(kind=8) |], [t| CLLong          |])
--    , ([ty| integer(kind=1) |], [t| CChar           |])
--    , ([ty| integer(kind=4) |], [t| CSize           |])
    , ([ty| integer(kind=1) |], [t| Int8            |])
    , ([ty| integer(kind=2) |], [t| Int16           |])
    , ([ty| integer(kind=4) |], [t| Int32           |])
    , ([ty| integer(kind=8) |], [t| Int64           |])
    , ([ty| real(kind=4)    |], [t| Float           |])
    , ([ty| complex(kind=4) |], [t| CComplex Float  |])
    , ([ty| complex(kind=8) |], [t| CComplex Double |])
    -}
    ]

-- | Basic unboxed Haskell types
--
-- TODO: MutableByteArray#
ghcUnboxed :: Q Context
ghcUnboxed = do
  mkContext "ghcUnboxed"
    [ ([ty| char      |], [t| Char#      |], False, "char  ")
    , ([ty| isize     |], [t| Int#       |], False, "isize ")
    , ([ty| usize     |], [t| Word#      |], False, "usize ")
    , ([ty| f32       |], [t| Float#     |], False, "f32   ")
    , ([ty| f64       |], [t| Double#    |], False, "f64   ")
--    , ([ty| *const i8 |], [t| ByteArray# |], False)
    ]

-- | Haskell pointers map onto Rust pointers. Note that unlike Rust, Haskell
-- doesn't really distinguish between pointers pointing to immutable memory from
-- those pointing to to mutable memory, so it is up to the user to enforce this.
--
-- NOTE: pointers will not support pointed types that require an intermediate
--       Rust type.
  {-
fImmutableVectors :: Q Context
fImmutableVectors = do
  vecConT <- [t| V.Vector |]
  pure (Context ([rule], [rev vecConT], [], "fImmutableVectors"))
  where
  rule vec context = do
    FArray _ t _  <- pure vec
    (t', Nothing) <- lookupRTypeInContext t context
    pure ([t| $t' |], Nothing)

  rev vecConT pt context = do
    AppT vecCon t <- pure pt
    if vecCon /= vecConT
      then mempty
      else do
        t' <- lookupHTypeInContext t context
        pure (FArray (-1) <$> t' <*> pure ())
        --pure (Ptr Mutable <$> t' <*> pure ())
-}

fComplex :: Q Context
fComplex = do
  vecConT <- [t| Complex |]
  pure (Context ([rule], [rev vecConT], [], "fComplex"))
  where
  rule vec context = do
    ff <- pure vec
    case ff of
      (FArray _ t _) -> do
        (t', Nothing,i) <- lookupRTypeInContext t context
        pure ([t| Ptr $t' |], Nothing,i)
      _ -> mempty

  rev vecConT pt context = do
    AppT vecCon t <- pure pt
    if vecCon /= vecConT
      then mempty
      else do
        t' <- lookupHTypeInContext t context
        pure (FArray (-1) <$> t' <*> pure ())
        --pure (Ptr Mutable <$> t' <*> pure ())

fVectors :: Q Context
fVectors = do
  vecConT <- [t| VM.MVector |]
  pure (Context ([rule], [rev vecConT], [], "fVectors"))
  where
  rule vec context = do
    ff <- pure vec
    case ff of
      (FArray _ t _) -> do
        (t', Nothing,i) <- lookupRTypeInContext t context
        pure ([t| Ptr $t' |], Nothing,i)
      _ -> mempty

  rev vecConT pt context = do
    AppT vecCon t <- pure pt
    if vecCon /= vecConT
      then mempty
      else do
        t' <- lookupHTypeInContext t context
        pure (FArray (-1) <$> t' <*> pure ())
        --pure (Ptr Mutable <$> t' <*> pure ())

vectors :: Q Context
vectors = do
  vecConT <- [t| V.MVector |]
  pure (Context ([rule], [rev vecConT], [], "vectors"))
  where
  rule vec context = do
    Array t _ _  <- pure vec
--    V.MVector t _  <- pure vec
    (t', Nothing,i) <- lookupRTypeInContext t context
    pure ([t| Ptr $t' |], Nothing,i)

  rev vecConT pt context = do
    AppT vecCon t <- pure pt
    if vecCon /= vecConT
      then mempty
      else do
        t' <- lookupHTypeInContext t context
        pure (Ptr Mutable <$> t' <*> pure ())

pointers :: Q Context
pointers = do
    ptrConT <- [t| Ptr |]
    pure (Context ([rule],[rev ptrConT],[constPtr,mutPtr],"pointers"))
  where
  rule pt context = do
    ff <- pure pt
    case ff of
      Ptr _ t _ -> do
        (t', Nothing,i) <- lookupRTypeInContext t context
        pure ([t| Ptr $t' |], Nothing,i)
      _ -> mempty

  rev ptrConT pt context = do
    AppT ptrCon t <- pure pt
    if ptrCon /= ptrConT
      then mempty
      else do
        t' <- lookupHTypeInContext t context
        pure (Ptr Mutable <$> t' <*> pure ())

  constPtr = unlines [ "impl<T> MarshalInto<*const T> for *const T {"
                     , "  fn marshal(self) -> *const T { self }"
                     , "}"
                     ]

  mutPtr =   unlines [ "impl<T> MarshalInto<*mut T> for *mut T {"
                     , "  fn marshal(self) -> *mut T { self }"
                     , "}"
                     ]

-- | This maps a Rust function type into the corresponding 'FunPtr' wrapped
-- Haskell function type.
--
-- Note that as a user, you are still responsible for marshalling values of
-- type 'FunPtr'. The reason for this is simple: the GHC runtime has no way of
-- automatically detecting when a pointer to a function is no longer present on
-- the Rust side.
--
-- NOTE: function pointers will not support pointed types that require an intermediate
--       Rust type.

makeIO :: Q HType -> Q HType
makeIO x = do
  ioT <- [t| IO |]
  x1 <- x
  xx <- case x1 of
          AppT i _ -> if i == ioT then x
                                  else [t| IO $x |]
          _ -> [t| IO $x |]
  return xx

functions :: Q Context
functions = do
  funPtrT <- [t| FunPtr |]
  ioT <- [t| IO |]
  pure (Context ([rule], [rev funPtrT ioT], [impl],"functions"))
  where
  rule :: RType -> Context -> First (Q HType, Maybe (Q RType),String)
  rule ft context = do
    --BareFn _ C _ (FnDecl args retTy False _) _ <- pure ft
    ff <- pure ft
    case ff of
      (FProcedurePtr _ retTy argTys _) -> do
        args' <- for argTys $ \arg -> do
          (t',_,_) <- lookupRTypeInContext arg context
          pure t'
        retTy' <- do
          (t',_,_) <- lookupRTypeInContext retTy context
          pure t'
        --retTy2 <- pure $ makeIO <$> retTy'
        let retTy2 :: Q HType
            retTy2 = makeIO retTy'
        let hFunTy = foldr (\l r -> [t| $l -> $r |]) retTy2 args'
        let hFunPtr = [t| FunPtr $hFunTy |]

        pure (hFunPtr, Nothing,"FProcedurePtr")
      _ -> mempty
  rev :: Type -> Type -> HType -> Context -> First (Q RType)
  rev funPtrT ioT ft context = do
    AppT funPtr t <- pure ft
    if funPtr /= funPtrT
      then mempty
      else do
        let ts = getApps t
            args = init ts

            ret   = last ts
            ret'  = case ret of
                      AppT io r | io == ioT -> r
                      r -> r
            ret'' = case ret' of
                      TupleT 0 -> Nothing
                      r -> Just r


        argsRs <- traverse (`lookupHTypeInContext` context) args
        retRs <- traverse (`lookupHTypeInContext` context) ret''
        let argsRs' :: Q [Arg ()]
            argsRs' = map (\a -> Arg Nothing a ()) <$> sequence argsRs
--          argsRs2 :: Q [RType]
--          argsRs2 = sequence argsRs
        let decl = FnDecl <$> argsRs' <*> sequence retRs <*> pure False <*> pure ()
        pure (BareFn Normal C [] <$> decl <*> pure ())
--        pure (FProcedurePtr "dummy" retRs argsRs2 <*> pure mempty )
--

  getApps :: Type -> [Type]
  getApps (AppT e1 e2) = e1 : getApps e2
  getApps e = [e]

  -- TODO: this only goes up to 16
  impl :: String
  impl = unlines (map implN [1..16])

    where
    vars = [ l : i | i <- "" : map show [(1 :: Int)..], l <- ['T'..'Z'] ]

    implN :: Int -> String
    implN n = let vs = intercalate "," (take n vars)
                  f = "extern \"C\" fn (" ++ vs ++ ") -> R"
              in unlines [ "impl<" ++ vs ++ ",R> MarshalInto<" ++ f ++ "> for (" ++ f ++ ") {"
                         , "  fn marshal(self) -> (" ++ f ++ ") { self }"
                         , "}"
                         ]


