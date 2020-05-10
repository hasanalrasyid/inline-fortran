--------------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}

--------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE EmptyDataDecls            #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ForeignFunctionInterface  #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UnboxedTuples             #-}
{-# LANGUAGE UndecidableInstances      #-}

--------------------------------------------------------------------------------

-- | Internal module to Eigen.
--   Here we define all foreign function calls,
--   and some typeclasses integral to the public and private interfaces
--   of the library.
module Eigen.Internal where --   FIXME: Explicit export list

--------------------------------------------------------------------------------

--import           Control.Monad            (when)
import           Data.Binary              (Binary(put,get))
import           Data.Binary.Get          (getByteString, getWord32be)
import           Data.Binary.Put          (putByteString, putWord32be)
import           Data.Bits                (xor)
import           Data.Complex             (Complex((:+)))
import           Data.Kind                (Type)
import           Data.Proxy               (Proxy(Proxy))
--import           Foreign.C.String         (CString, peekCString)
import           Foreign.C.Types          (CInt(CInt), CFloat(CFloat), CDouble(CDouble), CChar)
import           Foreign.ForeignPtr       (ForeignPtr, castForeignPtr, withForeignPtr)
import           Foreign.Ptr              (Ptr, castPtr, plusPtr)
import           Foreign.Storable         (Storable(sizeOf, alignment, poke, peek, peekByteOff, peekElemOff, pokeByteOff, pokeElemOff))
import           GHC.TypeLits             (natVal, KnownNat, Nat)
import           System.IO.Unsafe         (unsafeDupablePerformIO)
import qualified Data.Vector.Storable     as VS
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI

--------------------------------------------------------------------------------

-- | Like 'Proxy', but specialised to 'Nat'.
data Row (r :: Nat) = Row
-- | Like 'Proxy', but specialised to 'Nat'.
data Col (c :: Nat) = Col

-- | Used internally. Given a 'KnownNat' constraint, turn the type-level 'Nat' into an 'Int'.
natToInt :: forall n. KnownNat n => Int
{-# INLINE natToInt #-}
natToInt = fromIntegral (natVal @n Proxy)

--------------------------------------------------------------------------------

-- | Cast to and from a C-FFI type
--   'Cast' is a closed typeclass with an associated injective type family.
--   It is closed in the sense that we provide only four types
--   with instances for it; and intend for eigen to only be used
--   with those four types. The injectivity of the type family is
--   then useful for avoiding MPTCs. 'Cast' has two functions; 'toC'
--   and 'fromC', where 'toC' goes from a Haskell type to its associated
--   C type for internal use, with the C FFI, and 'fromC' goes from the
--   associated C type to the Haskell type.
class Cast (a :: Type) where
  type family C a = (result :: Type) | result -> a
  toC   :: a -> C a
  fromC :: C a -> a

instance Cast Int where
  type C Int = CInt
  toC = CInt . fromIntegral
  {-# INLINE toC #-}
  fromC (CInt x) = fromIntegral x
  {-# INLINE fromC #-}

instance Cast Float where
  type C Float = CFloat
  toC = CFloat
  {-# INLINE toC #-}
  fromC (CFloat x) = x
  {-# INLINE fromC #-}

instance Cast Double where
  type C Double = CDouble
  toC = CDouble
  {-# INLINE toC #-}
  fromC (CDouble x) = x
  {-# INLINE fromC #-}

instance Cast a => Cast (Complex a) where
  type C (Complex a) = CComplex (C a)
  toC (a :+ b) = CComplex (toC a) (toC b)
  {-# INLINE toC #-}
  fromC (CComplex a b) = (fromC a) :+ (fromC b)
  {-# INLINE fromC #-}

-- | WARNING! 'toC' is lossy for any Int greater than (maxBound :: Int32)!
instance Cast a => Cast (Int, Int, a) where
  type C (Int, Int, a) = CTriplet a
  {-# INLINE toC #-}
  toC (x, y, z) = CTriplet (toC x) (toC y) (toC z)
  {-# INLINE fromC #-}
  fromC (CTriplet x y z) = (fromC x, fromC y, fromC z)

--------------------------------------------------------------------------------

-- | Complex number for FFI with the same memory layout as std::complex\<T\>
data CComplex a = CComplex !a !a deriving (Show)

instance Storable a => Storable (CComplex a) where
    sizeOf _ = sizeOf (undefined :: a) * 2
    alignment _ = alignment (undefined :: a)
    poke p (CComplex x y) = do
        pokeElemOff (castPtr p) 0 x
        pokeElemOff (castPtr p) 1 y
    peek p = CComplex
        <$> peekElemOff (castPtr p) 0
        <*> peekElemOff (castPtr p) 1

--------------------------------------------------------------------------------

-- | FIXME: Doc
data CTriplet a where
  CTriplet :: Cast a => !CInt -> !CInt -> !(C a) -> CTriplet a

deriving instance (Show a, Show (C a)) => Show (CTriplet a)

instance (Storable a, Elem a) => Storable (CTriplet a) where
    sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: CInt) * 2
    alignment _ = alignment (undefined :: CInt)
    poke p (CTriplet row col val) = do
        pokeElemOff (castPtr p) 0 row
        pokeElemOff (castPtr p) 1 col
        pokeByteOff p (sizeOf (undefined :: CInt) * 2) val
    peek p = CTriplet
        <$> peekElemOff (castPtr p) 0
        <*> peekElemOff (castPtr p) 1
        <*> peekByteOff p (sizeOf (undefined :: CInt) * 2)

--------------------------------------------------------------------------------

-- | `Elem` is a closed typeclass that encompasses the properties
--   eigen expects its values to possess, and simplifies the external
--   API quite a bit.
class (Num a, Cast a, Storable a, Storable (C a), Code (C a)) => Elem a

instance Elem Float
instance Elem Double
instance Elem (Complex Float)
instance Elem (Complex Double)

--------------------------------------------------------------------------------

-- | Encode a C Type as a CInt
--
--   Hack used in FFI wrapper functions when constructing FFI calls
class Code a where; code :: a -> CInt
instance Code CFloat             where; code _ = 0
instance Code CDouble            where; code _ = 1
instance Code (CComplex CFloat)  where; code _ = 2
instance Code (CComplex CDouble) where; code _ = 3

-- | Hack used in constructing FFI calls.
newtype MagicCode = MagicCode CInt deriving Eq

instance Binary MagicCode where
    put (MagicCode _code) = putWord32be $ fromIntegral _code
    get = MagicCode . fromIntegral <$> getWord32be

-- | Hack used in constructing FFI calls.
magicCode :: Code a => a -> MagicCode
magicCode x = MagicCode (code x `xor` 0x45696730)

--------------------------------------------------------------------------------

-- | Machine size of a 'CInt'.
intSize :: Int
intSize = sizeOf (undefined :: CInt)

-- | FIXME: Doc
encodeInt :: CInt -> BS.ByteString
encodeInt x = BSI.unsafeCreate (sizeOf x) $ (`poke` x) . castPtr

-- | FIXME: Doc
decodeInt :: BS.ByteString -> CInt
decodeInt (BSI.PS fp fo fs)
    | fs == sizeOf x = x
    | otherwise = error "decodeInt: wrong buffer size"
    where x = performIO $ withForeignPtr fp $ peek . (`plusPtr` fo)

--------------------------------------------------------------------------------

-- | 'Binary' instance for 'Data.Vector.Storable.Mutable.Vector'
instance Storable a => Binary (VS.Vector a) where
    put vs = put (BS.length bs) >> putByteString bs where
        (fp,fs) = VS.unsafeToForeignPtr0 vs
        es = sizeOf (VS.head vs)
        bs = BSI.fromForeignPtr (castForeignPtr fp) 0 (fs * es)

    get = get >>= getByteString >>= \bs -> let
        (fp,fo,fs) = BSI.toForeignPtr bs
        es = sizeOf (VS.head vs)
        -- `plusForeignPtr` is used qualified here to just remind a reader
        -- that it is defined internally within eigen
        vs = VS.unsafeFromForeignPtr0 (Eigen.Internal.plusForeignPtr fp fo) (fs `div` es)
        in return vs

--------------------------------------------------------------------------------

-- | FIXME: Doc
data CSparseMatrix a
-- | FIXME: Doc
type CSparseMatrixPtr a = Ptr (CSparseMatrix a)

-- | FIXME: Doc
data CSolver a
-- | FIXME: Doc
type CSolverPtr a = Ptr (CSolver a)

-- {-# INLINE unholyPerformIO #-}
-- unholyPerformIO :: IO a -> a
-- unholyPerformIO (IO m) = case m realWorld# of (# _, r #) -> r

-- | FIXME: replace with unholyPerformIO (?)
performIO :: IO a -> a
performIO = unsafeDupablePerformIO

-- | FIXME: Doc
plusForeignPtr :: ForeignPtr a -> Int -> ForeignPtr b
plusForeignPtr fp fo = castForeignPtr fp1 where
    vs :: VS.Vector CChar
    vs = VS.unsafeFromForeignPtr (castForeignPtr fp) fo 0
    (fp1, _) = VS.unsafeToForeignPtr0 vs

--------------------------------------------------------------------------------
