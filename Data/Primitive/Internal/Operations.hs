{-# LANGUAGE MagicHash, ForeignFunctionInterface, UnliftedFFITypes #-}

-- |
-- Module      : Data.Primitive.Internal.Operations
-- Copyright   : (c) Roman Leshchinskiy 2011
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
-- 
-- Internal operations
--


module Data.Primitive.Internal.Operations (
  setWord8Array#, setWord16Array#, setWord32Array#,
  setWord64Array#, setWordArray#,
  setInt8Array#, setInt16Array#, setInt32Array#,
  setInt64Array#, setIntArray#,
  setAddrArray#, setFloatArray#, setDoubleArray#, setWideCharArray#,

  setWord8OffAddr#, setWord16OffAddr#, setWord32OffAddr#,
  setWord64OffAddr#, setWordOffAddr#,
  setInt8OffAddr#, setInt16OffAddr#, setInt32OffAddr#,
  setInt64OffAddr#, setIntOffAddr#,
  setAddrOffAddr#, setFloatOffAddr#, setDoubleOffAddr#, setWideCharOffAddr#
) where

import Data.Primitive.MachDeps (Word64_#, Int64_#)
import GHC.Prim

foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setWord8Array# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setWord16Array# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setWord32Array# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setWord64Array# :: MutableByteArray# s -> Int# -> Int# -> Word64_# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setWordArray# :: MutableByteArray# s -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setInt8Array# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setInt16Array# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setInt32Array# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setInt64Array# :: MutableByteArray# s -> Int# -> Int# -> Int64_# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setIntArray# :: MutableByteArray# s -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setAddrArray# :: MutableByteArray# s -> Int# -> Int# -> Addr# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Float"
  setFloatArray# :: MutableByteArray# s -> Int# -> Int# -> Float# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Double"
  setDoubleArray# :: MutableByteArray# s -> Int# -> Int# -> Double# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Char"
  setWideCharArray# :: MutableByteArray# s -> Int# -> Int# -> Char# -> IO ()

foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setWord8OffAddr# :: Addr# -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setWord16OffAddr# :: Addr# -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setWord32OffAddr# :: Addr# -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setWord64OffAddr# :: Addr# -> Int# -> Int# -> Word64_# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setWordOffAddr# :: Addr# -> Int# -> Int# -> Word# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word8"
  setInt8OffAddr# :: Addr# -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word16"
  setInt16OffAddr# :: Addr# -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word32"
  setInt32OffAddr# :: Addr# -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word64"
  setInt64OffAddr# :: Addr# -> Int# -> Int# -> Int64_# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Word"
  setIntOffAddr# :: Addr# -> Int# -> Int# -> Int# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Ptr"
  setAddrOffAddr# :: Addr# -> Int# -> Int# -> Addr# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Float"
  setFloatOffAddr# :: Addr# -> Int# -> Int# -> Float# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Double"
  setDoubleOffAddr# :: Addr# -> Int# -> Int# -> Double# -> IO ()
foreign import ccall unsafe "primitive-memops.h hsprimitive_memset_Char"
  setWideCharOffAddr# :: Addr# -> Int# -> Int# -> Char# -> IO ()

