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

module Language.Rust.Inline.Context where

import Language.Rust.Inline.Pretty ( renderType )

import Language.Rust.Syntax        ( Ty(BareFn, Ptr), Abi(..), FnDecl(..), Arg(..) )
import Language.Fortran.Quote         ( ty )

import Language.Haskell.TH

import Data.Semigroup              ( Semigroup )
import Data.Monoid                 ( First(..) )
import Data.Typeable               ( Typeable )
import Control.Monad               ( void )
import Data.Traversable            ( for )

import Data.Int                    ( Int8, Int16, Int32, Int64 )
import Data.Word                   ( Word8, Word16, Word32, Word64 )
import Foreign.Ptr                 ( Ptr, FunPtr )
import Foreign.C.Types             -- pretty much every type here is used
import qualified Control.Monad.Fail as Fail
import Eigen.Internal -- CComplex

instance Fail.MonadFail First where
  fail = error "MonadFail First error"
-- Easier on the eyes
type RType = Ty ()
type HType = Type

-- | Represents a prioritized set of rules for converting a Rust type to a
-- Haskell one. The 'Context' argument encodes the fact that we may need look
-- recursively into the 'Context' again before possibly producing a Haskell
-- type.
newtype Context = Context [ RType -> Context -> First (Q HType) ]
  deriving (Semigroup, Monoid, Typeable)

-- | Search in a 'Context' for the Haskell type corresponding to a Rust type.
-- The approach taken is to scan the context rules from left to right looking
-- for the first successful conversion to a Haskell type.
--
-- It is expected that the 'HType' found from an 'RType' have a 'Storable'
-- instance which has the memory layout of 'RType'.
lookupTypeInContext :: RType -> Context -> First (Q (HType))
lookupTypeInContext rustType context@(Context rules) =
  foldMap (\fits -> fits rustType context) rules


-- | Partial version of 'lookupTypeInContext' that fails with an error message
-- if the type is not convertible.
getTypeInContext :: RType -> Context -> Q HType
getTypeInContext rustType context =
  case getFirst (lookupTypeInContext rustType context) of
    Just hType -> hType
    Nothing -> fail $ unwords [ "Could not find information about"
                              , renderType rustType
                              , "in the context of"
                              , show rustType
                              ]


-- | Make a 'Context' consisting of rules to map the Rust types on the left to
-- the Haskell types on the right.
mkContext :: [(Ty a, Q HType)] -> Context
mkContext = Context . map fits
  where
    fits (rts, qht) rt _ | rt == void rts = pure qht
                         | otherwise = mempty

-- | Make a singleton 'Context' consisting of a rule to map the given Rust type
-- to the given Haskell type.
singleton :: Ty a -> Q HType -> Context
singleton rts qht = mkContext [(rts, qht)]


-- * Some handy contexts

-- | Types defined in 'Foreign.C.Types' and the 'libc' crate.
--
-- There should be no conversion required here - these have /identical/ memory
-- layouts (since they both promise to have the same memory layout as C) and are
-- passed on the stack.
libc :: Context
libc = mkContext
  [ ([ty| libc::c_char      |], [t| CChar      |]) -- char
  , ([ty| libc::c_schar     |], [t| CSChar     |]) -- signed char
  , ([ty| libc::c_uchar     |], [t| CUChar     |]) -- unsigned char
  , ([ty| libc::c_short     |], [t| CShort     |]) -- short
  , ([ty| libc::c_ushort    |], [t| CUShort    |]) -- unsigned short
  , ([ty| libc::c_int       |], [t| CInt       |]) -- int
  , ([ty| libc::c_uint      |], [t| CUInt      |]) -- unsigned int
  , ([ty| libc::c_long      |], [t| CLong      |]) -- long
  , ([ty| libc::c_ulong     |], [t| CULong     |]) -- unsigned long
  , ([ty| libc::ptrdiff_t   |], [t| CPtrdiff   |]) -- ptrdiff_t
  , ([ty| libc::size_t      |], [t| CSize      |]) -- size_t
  , ([ty| libc::wchar_t     |], [t| CWchar     |]) -- wchar_t
  , ([ty| libc::c_longlong  |], [t| CLLong     |]) -- long long
  , ([ty| libc::c_ulonglong |], [t| CULLong    |]) -- unsigned long long
  , ([ty| libc::boolean_t   |], [t| CBool      |]) -- bool
  , ([ty| libc::intptr_t    |], [t| CIntPtr    |]) -- intptr_t
  , ([ty| libc::uintptr_t   |], [t| CUIntPtr   |]) -- uintptr_t
  , ([ty| libc::intmax_t    |], [t| CIntMax    |]) -- intmax_t
  , ([ty| libc::uintmax_t   |], [t| CUIntMax   |]) -- unsigned intmax_t
  , ([ty| libc::clock_t     |], [t| CClock     |]) -- clock_t
  , ([ty| libc::time_t      |], [t| CTime      |]) -- time_t
  , ([ty| libc::useconds_t  |], [t| CUSeconds  |]) -- useconds_t
  , ([ty| libc::suseconds_t |], [t| CSUSeconds |]) -- suseconds_t
  , ([ty| libc::c_float     |], [t| CFloat     |]) -- float
  , ([ty| libc::c_double    |], [t| CDouble    |]) -- double
  , ([ty| libc::FILE        |], [t| CFile      |]) -- FILE
  , ([ty| libc::fpos_t      |], [t| CFpos      |]) -- fpos_t
  , ([ty| libc::int8_t      |], [t| Int8       |]) -- int8_t
  , ([ty| libc::int16_t     |], [t| Int16      |]) -- int16_t
  , ([ty| libc::int32_t     |], [t| Int32      |]) -- int32_t
  , ([ty| libc::int64_t     |], [t| Int64      |]) -- int64_t
  , ([ty| libc::uint8_t     |], [t| Word8      |]) -- uint8_t
  , ([ty| libc::uint16_t    |], [t| Word16     |]) -- uint16_t
  , ([ty| libc::uint32_t    |], [t| Word32     |]) -- uint32_t
  , ([ty| libc::uint64_t    |], [t| Word64     |]) -- uint64_t
  ]

-- | Basic numeric (and similar) Haskell and Rust types.
--
-- There should be no conversion required here as these should have identical
-- memory layouts.
basic :: Context
basic = mkContext
  [ ([ty| bool  |], [t| Word8   |])
  , ([ty| char  |], [t| Char    |]) -- 4 bytes
  , ([ty| i8    |], [t| Int8    |])
  , ([ty| i16   |], [t| Int16   |])
  , ([ty| i32   |], [t| Int32   |])
  , ([ty| i64   |], [t| Int64   |])
  , ([ty| u8    |], [t| Word8   |])
  , ([ty| u16   |], [t| Word16  |])
  , ([ty| u32   |], [t| Word32  |])
  , ([ty| u64   |], [t| Word64  |])
  , ([ty| f32   |], [t| Float   |])
  , ([ty| f64   |], [t| Double  |])
  , ([ty| isize |], [t| Int     |])
  , ([ty| usize |], [t| Word    |])
  , ([ty| ()    |], [t| ()      |])
  , ([ty| integer         |], [t| Int32           |])
  , ([ty| real            |], [t| Float           |])
  , ([ty| logical         |], [t| Int8            |])
  , ([ty| real            |], [t| Float           |])
  , ([ty| complex         |], [t| CComplex Float  |])
  , ([ty| character       |], [t| CChar           |])
-- Fortran
  {-
  , ([ty| logical(kind=1) |], [t| Int8            |])
  , ([ty| character(len=1)|], [t| CChar           |])
--  , ([ty| integer         |], [t| CInt            |])
--  , ([ty| integer(kind=2) |], [t| CShort          |])
--  , ([ty| integer(kind=4) |], [t| CInt            |])
--  , ([ty| integer(kind=4) |], [t| CLong           |])
--  , ([ty| integer(kind=8) |], [t| CLLong          |])
--  , ([ty| integer(kind=1) |], [t| CChar           |])
--  , ([ty| integer(kind=4) |], [t| CSize           |])
  , ([ty| integer(kind=1) |], [t| Int8            |])
  , ([ty| integer(kind=2) |], [t| Int16           |])
  , ([ty| integer(kind=4) |], [t| Int32           |])
  , ([ty| integer(kind=8) |], [t| Int64           |])
  , ([ty| real(kind=4)    |], [t| Float           |])
  , ([ty| real(kind=8)    |], [t| Double          |])
  , ([ty| complex(kind=4) |], [t| CComplex Float  |])
  , ([ty| complex(kind=8) |], [t| CComplex Double |])
  -}
  ]
    {- Interop C Fortran
C_BOOL                      _Bool                   logical(kind=1)           Int8
                                                    character                 CChar
C_CHAR                      char                    character(len=1)          CChar
                                                    integer                   CInt
C_SHORT                     short int               integer(kind=2)           CShort
C_INT                       int                     integer(kind=4)           CInt      Int32
C_LONG                      long int                integer(kind=4 or 8)      CLong     Int32/64
C_LONG_LONG                 long long int           integer(kind=8)           CLLong    Int64
C_SIGNED_CHAR               u/signed char           integer(kind=1)           CChar     Int8
C_SIZE_T                    size_t                  integer(kind=4 or 8)      CSize     Int32/64
C_INT8_T                    int8_t                  integer(kind=1)           Int8
C_INT16_T                   int16_t                 integer(kind=2)           Int16
C_INT32_T                   int32_t                 integer(kind=4)           Int32
C_INT64_T                   int64_t                 integer(kind=8)           Int64
                                                    real                      Float
C_FLOAT                     float                   real(kind=4)              Float
C_DOUBLE                    double                  real(kind=8)              Double
C_LONG_DOUBLE               long double             real(kind=8 or 16)        Double            -- we do not implement LongDouble
                                                    complex                   CComplex Float
C_FLOAT_COMPLEX             float _Complex          complex(kind=4)           CComplex Float    -- CComplex provided by Data.Eigen.Matrix
C_DOUBLE_COMPLEX            double _Complex         complex(kind=8)           CComplex Double
C_LONG_DOUBLE_COMPLEX       long double _Complex    complex(kind=8 or 16)     CComplex Double
       -}


-- | Haskell pointers map onto Rust pointers. Note that unlike Rust, Haskell
-- doesn't really distinguish between pointers pointing to immutable memory from
-- those pointing to to mutable memory, so it is up to the user to enforce this.
pointers :: Context
pointers = Context [ rule ]
  where
  rule pt context = do
    Ptr _ t _ <- pure pt
    t' <- lookupTypeInContext t context
    pure [t| Ptr $t' |]

-- | This maps a Rust function type into the corresponding 'FunPtr' wrapped
-- Haskell function type.
--
-- Note that as a user, you are still responsible for marshalling values of
-- type 'FunPtr'. The reason for this is simple: the GHC runtime has no way of
-- automatically detecting when a pointer to a function is no longer present on
-- the Rust side.
functions :: Context
functions = Context [ rule ]
  where
  rule ft context = do
    BareFn _ C _ (FnDecl args retTy False _) _ <- pure ft
    args' <- for args $ \arg -> do
               Arg _ argTy _ <- pure arg
               lookupTypeInContext argTy context

    retTy' <- case retTy of
                Just t -> lookupTypeInContext t context
                Nothing -> pure [t| IO () |]

    let hFunTy = foldr (\l r -> [t| $l -> $r |]) retTy' args'
    let hFunPtr = [t| FunPtr $hFunTy |]

    pure hFunPtr
