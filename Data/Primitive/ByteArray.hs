{-# LANGUAGE MagicHash, UnboxedTuples, ForeignFunctionInterface,
             UnliftedFFITypes, DeriveDataTypeable #-}

-- |
-- Module      : Data.Primitive.ByteArray
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
-- 
-- Primitive operations on ByteArrays
--

module Data.Primitive.ByteArray (
  ByteArray(..), MutableByteArray(..), ByteArray#, MutableByteArray#,

  newByteArray, newPinnedByteArray, newAlignedPinnedByteArray,
  readByteArray, writeByteArray, indexByteArray,
  unsafeFreezeByteArray, unsafeThawByteArray,
  sizeofByteArray, sizeofMutableByteArray, sameMutableByteArray,
  byteArrayContents, mutableByteArrayContents,
  copyByteArray, copyMutableByteArray, moveByteArray, fillByteArray,

  -- * Deprecated operations
  memcpyByteArray, memcpyByteArray', memmoveByteArray, memsetByteArray
) where

import Control.Monad.Primitive
import Data.Primitive.Types

import Foreign.C.Types
import Data.Word ( Word8 )
import GHC.Base ( Int(..) )
import GHC.Prim

import Data.Typeable ( Typeable )
import Data.Data ( Data(..) )
import Data.Primitive.Internal.Compat ( mkNoRepType )

-- | Byte arrays
data ByteArray = ByteArray ByteArray# deriving ( Typeable )

-- | Mutable byte arrays associated with a primitive state token
data MutableByteArray s = MutableByteArray (MutableByteArray# s)
                                        deriving( Typeable )

-- | Create a new mutable byte array of the specified size.
newByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
{-# INLINE newByteArray #-}
newByteArray (I# n#)
  = primitive (\s# -> case newByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size. The garbage collector
-- is guaranteed not to move it.
newPinnedByteArray :: PrimMonad m => Int -> m (MutableByteArray (PrimState m))
{-# INLINE newPinnedByteArray #-}
newPinnedByteArray (I# n#)
  = primitive (\s# -> case newPinnedByteArray# n# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Create a /pinned/ byte array of the specified size and with the give
-- alignment. The garbage collector is guaranteed not to move it.
newAlignedPinnedByteArray
  :: PrimMonad m => Int -> Int -> m (MutableByteArray (PrimState m))
{-# INLINE newAlignedPinnedByteArray #-}
newAlignedPinnedByteArray (I# n#) (I# k#)
  = primitive (\s# -> case newAlignedPinnedByteArray# n# k# s# of
                        (# s'#, arr# #) -> (# s'#, MutableByteArray arr# #))

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays allocated by 'newPinnedByteArray' or
-- 'newAlignedPinnedByteArray'.
byteArrayContents :: ByteArray -> Addr
{-# INLINE byteArrayContents #-}
byteArrayContents (ByteArray arr#) = Addr (byteArrayContents# arr#)

-- | Yield a pointer to the array's data. This operation is only safe on
-- /pinned/ byte arrays allocated by 'newPinnedByteArray' or
-- 'newAlignedPinnedByteArray'.
mutableByteArrayContents :: MutableByteArray s -> Addr
{-# INLINE mutableByteArrayContents #-}
mutableByteArrayContents (MutableByteArray arr#)
  = Addr (byteArrayContents# (unsafeCoerce# arr#))

-- | Check if the two arrays refer to the same memory block.
sameMutableByteArray :: MutableByteArray s -> MutableByteArray s -> Bool
{-# INLINE sameMutableByteArray #-}
sameMutableByteArray (MutableByteArray arr#) (MutableByteArray brr#)
  = sameMutableByteArray# arr# brr#

-- | Convert a mutable byte array to an immutable one without copying. The
-- array should not be modified after the conversion.
unsafeFreezeByteArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> m ByteArray
{-# INLINE unsafeFreezeByteArray #-}
unsafeFreezeByteArray (MutableByteArray arr#)
  = primitive (\s# -> case unsafeFreezeByteArray# arr# s# of
                        (# s'#, arr'# #) -> (# s'#, ByteArray arr'# #))

-- | Convert an immutable byte array to a mutable one without copying. The
-- original array should not be used after the conversion.
unsafeThawByteArray
  :: PrimMonad m => ByteArray -> m (MutableByteArray (PrimState m))
{-# INLINE unsafeThawByteArray #-}
unsafeThawByteArray (ByteArray arr#)
  = primitive (\s# -> (# s#, MutableByteArray (unsafeCoerce# arr#) #))

-- | Size of the byte array.
sizeofByteArray :: ByteArray -> Int
{-# INLINE sizeofByteArray #-}
sizeofByteArray (ByteArray arr#) = I# (sizeofByteArray# arr#)

-- | Size of the mutable byte array.
sizeofMutableByteArray :: MutableByteArray s -> Int
{-# INLINE sizeofMutableByteArray #-}
sizeofMutableByteArray (MutableByteArray arr#) = I# (sizeofMutableByteArray# arr#)

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
indexByteArray :: Prim a => ByteArray -> Int -> a
{-# INLINE indexByteArray #-}
indexByteArray (ByteArray arr#) (I# i#) = indexByteArray# arr# i#

-- | Read a primitive value from the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
readByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> m a
{-# INLINE readByteArray #-}
readByteArray (MutableByteArray arr#) (I# i#)
  = primitive (readByteArray# arr# i#)

-- | Write a primitive value to the byte array. The offset is given in
-- elements of type @a@ rather than in bytes.
writeByteArray
  :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> a -> m ()
{-# INLINE writeByteArray #-}
writeByteArray (MutableByteArray arr#) (I# i#) x
  = primitive_ (writeByteArray# arr# i# x)

#if __GLASGOW_HASKELL__ >= 702
i# :: Int -> Int#
i# (I# n#) = n#
#endif

-- | Copy a slice of an immutable byte array to a mutable byte array.
copyByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ destination array
                 -> Int                 -- ^ offset into destination array
                 -> ByteArray           -- ^ source array
                 -> Int                 -- ^ offset into source array
                 -> Int                 -- ^ number of bytes to copy
                 -> m ()
{-# INLINE copyByteArray #-}
copyByteArray (MutableByteArray dst#) doff (ByteArray src#) soff sz
#if __GLASGOW_HASKELL__ >= 702
  = primitive_ (copyByteArray# src# (i# soff) dst# (i# doff) (i# sz))
#else
  = unsafePrimToPrim
  $ memcpy_ba dst# (fromIntegral doff) src# (fromIntegral soff)
                 (fromIntegral sz)
#endif

-- | Copy a slice of a mutable byte array into another array. The two slices
-- may not overlap.
copyMutableByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ destination array
                 -> Int                 -- ^ offset into destination array
                 -> MutableByteArray (PrimState m)
                                        -- ^ source array
                 -> Int                 -- ^ offset into source array
                 -> Int                 -- ^ number of bytes to copy
                 -> m ()
{-# INLINE copyMutableByteArray #-}
copyMutableByteArray (MutableByteArray dst#) doff
                     (MutableByteArray src#) soff sz
#if __GLASGOW_HASKELL__ >= 702
  = primitive_ (copyMutableByteArray# src# (i# soff) dst# (i# doff) (i# sz))
#else
  = unsafePrimToPrim
  $ memcpy_mba dst# (fromIntegral doff) src# (fromIntegral soff)
                    (fromIntegral sz)
#endif

-- | Copy a slice of a mutable byte array into another, potentially
-- overlapping array.
moveByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ destination array
                 -> Int                 -- ^ offset into destination array
                 -> MutableByteArray (PrimState m)
                                        -- ^ source array
                 -> Int                 -- ^ offset into source array
                 -> Int                 -- ^ number of bytes to copy
                 -> m ()
{-# INLINE moveByteArray #-}
moveByteArray (MutableByteArray dst#) doff
              (MutableByteArray src#) soff sz
  = unsafePrimToPrim
  $ memmove_mba dst# (fromIntegral doff) src# (fromIntegral soff)
                     (fromIntegral sz)

-- | Fill a slice of a mutable byte array with a value.
fillByteArray
  :: PrimMonad m => MutableByteArray (PrimState m)
                                        -- ^ array to fill
                 -> Int                 -- ^ offset into array
                 -> Int                 -- ^ number of bytes to fill
                 -> Word8               -- ^ value to fill with
                 -> m ()
{-# INLINE fillByteArray #-}
fillByteArray (MutableByteArray dst#) doff c sz
  = unsafePrimToPrim
  $ memset_mba dst# (fromIntegral doff) (fromIntegral c) (fromIntegral sz)



memcpyByteArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> Int
                 -> MutableByteArray (PrimState m) -> Int
                 -> Int -> m ()
{-# DEPRECATED memcpyByteArray "Use copyMutableByteArray instead" #-}
{-# INLINE memcpyByteArray #-}
memcpyByteArray = copyMutableByteArray

memcpyByteArray'
  :: PrimMonad m => MutableByteArray (PrimState m) -> Int
                 -> ByteArray -> Int
                 -> Int -> m ()
{-# DEPRECATED memcpyByteArray' "Use copyByteArray instead" #-}
{-# INLINE memcpyByteArray' #-}
memcpyByteArray' = copyByteArray

memmoveByteArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> Int
                 -> MutableByteArray (PrimState m) -> Int
                 -> Int -> m ()
{-# DEPRECATED memmoveByteArray "Use moveByteArray instead" #-}
{-# INLINE memmoveByteArray #-}
memmoveByteArray = moveByteArray

memsetByteArray
  :: PrimMonad m => MutableByteArray (PrimState m) -> Int -> Word8
                 -> Int -> m ()
{-# DEPRECATED memsetByteArray "Use fillByteArray instead (WARNING: order of arguments is different)" #-}
{-# INLINE memsetByteArray #-}
memsetByteArray dst off x sz = fillByteArray dst off sz x


foreign import ccall unsafe "primitive-memops.h memcpy_off"
  memcpy_mba :: MutableByteArray# s -> CInt
             -> MutableByteArray# s -> CInt
             -> CSize -> IO ()

foreign import ccall unsafe "primitive-memops.h memcpy_off"
  memcpy_ba :: MutableByteArray# s -> CInt
            -> ByteArray# -> CInt
            -> CSize -> IO ()

foreign import ccall unsafe "primitive-memops.h memmove_off"
  memmove_mba :: MutableByteArray# s -> CInt
              -> MutableByteArray# s -> CInt
              -> CSize -> IO ()

foreign import ccall unsafe "primitive-memops.h memset_off"
  memset_mba :: MutableByteArray# s -> CInt -> CInt -> CSize -> IO ()

instance Data ByteArray where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.ByteArray.ByteArray"

instance Typeable s => Data (MutableByteArray s) where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.ByteArray.MutableByteArray"

