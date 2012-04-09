{-# LANGUAGE UnboxedTuples, MagicHash, DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables, TypeOperators, DefaultSignatures, GADTs, TypeFamilies, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

-- |
-- Module      : Data.Primitive.Types
-- Copyright   : (c) Roman Leshchinskiy 2009-2010
-- License     : BSD-style
--
-- Maintainer  : Roman Leshchinskiy <rl@cse.unsw.edu.au>
-- Portability : non-portable
-- 
-- Basic types and classes for primitive array operations
--

module Data.Primitive.Types (
  Prim(..),
  indexByteArray#,
  readByteArray#,
  writeByteArray#,
  setByteArray#,
  indexOffAddr#,
  readOffAddr#,
  writeOffAddr#,
  setOffAddr#,

  Addr(..),
) where

import Control.Monad.Primitive
import Data.Primitive.MachDeps
import Data.Primitive.Internal.Operations

import GHC.Base (
    unsafeCoerce#,
    Int(..), Char(..),
  )
import GHC.Float (
    Float(..), Double(..)
  )
import GHC.Word (
    Word(..), Word8(..), Word16(..), Word32(..), Word64(..)
  )
import GHC.Int (
    Int8(..), Int16(..), Int32(..), Int64(..)
  )

import GHC.Prim
import GHC.Generics

import Data.Typeable ( Typeable )
import Data.Data ( Data(..) )
import Data.Primitive.Internal.Compat ( mkNoRepType )

-- | A machine address
data Addr = Addr Addr# deriving ( Typeable )

instance Eq Addr where
  Addr a# == Addr b# = eqAddr# a# b#
  Addr a# /= Addr b# = neAddr# a# b#

instance Ord Addr where
  Addr a# > Addr b# = gtAddr# a# b#
  Addr a# >= Addr b# = geAddr# a# b#
  Addr a# < Addr b# = ltAddr# a# b#
  Addr a# <= Addr b# = leAddr# a# b#

instance Data Addr where
  toConstr _ = error "toConstr"
  gunfold _ _ = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Primitive.Types.Addr"

-- | Read a value from the array. The offset is in elements of type
-- @a@ rather than in bytes.
indexByteArray# :: forall a. Prim a => ByteArray# -> Int# -> a
indexByteArray# arr# i# = indexByteArrayA# arr# (ix# (undefined :: a) i#)

-- | Read a value from the mutable array. The offset is in elements of type
-- @a@ rather than in bytes.
readByteArray# :: forall a s. Prim a => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
readByteArray# arr# i# s# = readByteArrayA# arr# (ix# (undefined :: a) i#) s#

-- | Write a value to the mutable array. The offset is in elements of type
-- @a@ rather than in bytes.
writeByteArray# :: forall a s. Prim a => MutableByteArray# s -> Int# -> a -> State# s -> State# s
writeByteArray# arr# i# s# = writeByteArrayA# arr# (ix# (undefined :: a) i#) s#

-- | Fill a slice of the mutable array with a value. The offset and length
-- of the chunk are in elements of type @a@ rather than in bytes.
setByteArray# :: forall a s. Prim a => MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s
setByteArray# arr# i# n# a s# = setByteArrayA# arr# (ix# (undefined :: a) i#) n# a s#

-- | Read a value from a memory position given by an address and an offset.
-- The memory block the address refers to must be immutable. The offset is in
-- elements of type @a@ rather than in bytes.
indexOffAddr# :: forall a. Prim a => Addr# -> Int# -> a
indexOffAddr# addr# i# = indexOffAddrA# addr# (ix# (undefined :: a) i#)

-- | Read a value from a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
readOffAddr# :: forall a s. Prim a => Addr# -> Int# -> State# s -> (# State# s, a #)
readOffAddr# addr# i# s# = readOffAddrA# addr# (ix# (undefined :: a) i#) s#

-- | Write a value to a memory position given by an address and an offset.
-- The offset is in elements of type @a@ rather than in bytes.
writeOffAddr# :: forall a s. Prim a => Addr# -> Int# -> a -> State# s -> State# s
writeOffAddr# addr# i# a s# = writeOffAddrA# addr# (ix# (undefined :: a) i#) a s#

-- | Fill a memory block given by an address, an offset and a length.
-- The offset and length are in elements of type @a@ rather than in bytes.
setOffAddr# :: forall a s. Prim a => Addr# -> Int# -> Int# -> a -> State# s -> State# s
setOffAddr# addr# i# n# a s# = setOffAddrA# addr# (ix# (undefined :: a) i#) n# a s#

-- | Converts an index in terms of \"elements of type @a@\" to an index in terms of @alignment a@. The first
-- argument is not used.
ix# :: forall a. Prim a => a -> Int# -> Int#
{-# INLINE ix# #-}
ix# _ i# = i# *# (quotInt# (sizeOf# (undefined :: a)) (alignment# (undefined :: a)))

-- | Class of types supporting primitive array operations.
--
-- With GHC 7.2 and above, product types may be automatically derived as instances
-- of 'Prim'. For example:
--
-- >{-# LANGUAGE DeriveGeneric #-}
-- >data Struct = Struct Word8 Int16 deriving Generic
-- >instance Prim Struct
--
-- Such values are stored in arrays just as structs are stored in arrays in C. For instance,
-- a list of values @[Struct w0 i0, Struct w1 i1, ...]@ will be stored in arrays as
--
-- > <w0 (1 byte)> <1 byte padding> <i0 (2 bytes)> <w1 (1 byte)> <1 byte padding> <i1 (2 bytes)> ...
--
-- The fields are written in the order they appear in the data type, so it
-- may be beneficial to order the fields in the data type to minimize wasted
-- space due to padding. For instance,
--
-- >data Struct1 = Struct1 Word8 Int64 Word8
--
-- will require 14 bytes of padding when stored in an array (7 bytes padding after each @Word8@),
-- whereas reordering the fields to
--
-- >data Struct2 = Struct2 Word8 Word8 Int64
--
-- reduces the padding to 6 bytes (all after the second @Word8@).
class Prim a where
  -- | Size of values of type @a@. This is required to be a multiple of 'alignment#'. The argument is not used.
  sizeOf#    :: a -> Int#

  -- | Alignment of values of type @a@. This must be a power of 2. The argument is not used.
  alignment# :: a -> Int#

  -- | Read a value from the array. The offset is in elements of size
  -- @alignment# a@ rather than in bytes.
  indexByteArrayA# :: ByteArray# -> Int# -> a

  -- | Read a value from the mutable array. The offset is in elements of size
  -- @alignment# a@ rather than in bytes.
  readByteArrayA# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to the mutable array. The offset is in elements of size
  -- @alignment# a@ rather than in bytes.
  writeByteArrayA# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s

  -- | Fill a slice of the mutable array with a value. The offset is
  -- in elements of size @alignment# a@ rather than in bytes. The length
  -- is in elements of type @a@.
  setByteArrayA# :: MutableByteArray# s -> Int# -> Int# -> a -> State# s -> State# s

  -- | Read a value from a memory position given by an address and an offset.
  -- The memory block the address refers to must be immutable. The offset is in
  -- elements of size @alignment# a@ rather than in bytes.
  indexOffAddrA# :: Addr# -> Int# -> a

  -- | Read a value from a memory position given by an address and an offset.
  -- The offset is in elements of size @alignment# a@ rather than in bytes.
  readOffAddrA# :: Addr# -> Int# -> State# s -> (# State# s, a #)

  -- | Write a value to a memory position given by an address and an offset.
  -- The offset is in elements of size @alignment# a@ rather than in bytes.
  writeOffAddrA# :: Addr# -> Int# -> a -> State# s -> State# s

  -- | Fill a memory block given by an address, an offset and a length.
  -- The offset and length are in elements of size @alignment# a@ rather than in bytes.
  setOffAddrA# :: Addr# -> Int# -> Int# -> a -> State# s -> State# s

  setByteArrayA# arr# i0# n0# a s0# =
    let
      loop# i# count# s#
        | count# ==# 0# = s#
        | otherwise = loop# (i# +# ix# (undefined :: a) 1#) (count# -# 1#) (writeByteArrayA# arr# i# a s#)
    in
     loop# i0# n0# s0#
#if __GLASGOW_HASKELL__ >= 702
  {-# INLINABLE setByteArrayA# #-}
#else
  {-# INLINE setByteArrayA# #-}
#endif

  setOffAddrA# addr# i0# n0# a s0# =
    let
      loop# i# count# s#
        | count# ==# 0# = s#
        | otherwise = loop# (i# +# ix# (undefined :: a) 1#) (count# -# 1#) (writeOffAddrA# addr# i# a s#)
    in
     loop# i0# n0# s0#
#if __GLASGOW_HASKELL__ >= 702
  {-# INLINABLE setOffAddrA# #-}
#else
  {-# INLINE setOffAddrA# #-}
#endif

#ifdef GENERICS
  default sizeOf# :: (Generic a, Product (Rep a), GPrim (Elems (Rep a))) => a -> Int#
  -- round up size to a multiple of alignment; see note on 'GPrim' class
  sizeOf# _ =
   let
    size# = gsizeOf# (undefined :: Elems (Rep a))
    align# = galignment# (undefined :: Elems (Rep a))
    r# = remInt# size# align#
    pad# = if r# ==# 0# then 0# else align# -# r#
   in
   size# +# pad#
  {-# INLINE sizeOf# #-}

  default alignment# :: (Generic a, Product (Rep a), GPrim (Elems (Rep a))) => a -> Int#
  alignment# _ = galignment# (undefined :: Elems (Rep a))
  {-# INLINE alignment# #-}

  default indexByteArrayA# :: (Generic a, Product (Rep a), GPrim (Elems (Rep a))) => ByteArray# -> Int# -> a
  indexByteArrayA# arr# i# = to (fromProd (gindexByteArrayA# arr# i#))
  {-# INLINE indexByteArrayA# #-}

  default readByteArrayA# :: (Generic a, Product (Rep a), GPrim (Elems (Rep a))) => MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  readByteArrayA# arr# i# s# = case greadByteArrayA# arr# i# s# of
    (# s1#, prod #) -> (# s1#, to (fromProd prod) #)
  {-# INLINE readByteArrayA# #-}

  default writeByteArrayA# :: (Generic a, Product (Rep a), GPrim (Elems (Rep a))) => MutableByteArray# s -> Int# -> a -> State# s -> State# s
  writeByteArrayA# arr# i# a s# = gwriteByteArrayA# arr# i# (toProd (from a)) s#
  {-# INLINE writeByteArrayA# #-}

  default indexOffAddrA# :: (Generic a, Product (Rep a), GPrim (Elems (Rep a))) => Addr# -> Int# -> a
  indexOffAddrA# addr# i# = to (fromProd (gindexOffAddrA# addr# i#))
  {-# INLINE indexOffAddrA# #-}

  default readOffAddrA# :: (Generic a, Product (Rep a), GPrim (Elems (Rep a))) => Addr# -> Int# -> State# s -> (# State# s, a #)
  readOffAddrA# addr# i# s# = case greadOffAddrA# addr# i# s# of
    (# s1#, prod #) -> (# s1#, to (fromProd prod) #)
  {-# INLINE readOffAddrA# #-}

  default writeOffAddrA# :: (Generic a, Product (Rep a), GPrim (Elems (Rep a))) => Addr# -> Int# -> a -> State# s -> State# s
  writeOffAddrA# addr# i# a s# = gwriteOffAddrA# addr# i# (toProd (from a)) s#
  {-# INLINE writeOffAddrA# #-}

{-
Approach to generics
--------------------

Take the example of

  data Struct = Struct Word8 Word8 Word8 Word32

We would like derive a 'Prim' instance which uses the following
padding:

  <Word8> <Word8> <Word8> <1 byte padding> <Word32>   (size 8, alignment 4)

In other words, we place fields from left to right, and insert padding when necessary.

The generic representation of Struct is roughly (after dropping M1 and K1 newtypes)

  Rep Struct = ((Word8 :*: Word8) :*: (Word8 :*: Word32))

If we calculated placements based on this balanced-tree-of-products
representation, we would get:

  Word8 :*: Word8                             -- <Word8> <Word8>   (size 2, alignment 1)
  Word8 :*: Word32                            -- <Word8> <3 bytes padding> <Word32>   (size 8, alignment 4)
  (Word8 :*: Word8) :*: (Word8 :*: Word32)    -- <Word8> <Word8> <2 bytes padding> <Word8> <3 bytes padding> <Word32> (size 12, alignment 4)

This is not what we want:

  * it wastes space relative to the "place fields from left to right" scheme

  * it is sensitive to the precise balancing of the products chosen by GHC, making the
    result unpredictable to users.

So, our approach is: 

  * first convert the balanced-tree-of-products representation provided by the Generic
    typeclass into a snoc-list-of-fields representation

  * now do field placement on this snoc-list-of-fields representation

For the Struct example above, we get:

  Rep Struct = ((Word8 :*: Word8) :*: (Word8 :*: Word32))
  Elems (Rep Struct) = (((Nil :* Word8) :* Word8) :* Word8) :* Word32

  -- converting to Rep    (from Generic typeclass)
  from :: Struct -> Rep Struct x
  to :: Rep Struct () -> Struct

  -- converting to Elems  (from Product typeclass)
  toProd :: Rep Struct () -> Elems (Rep Struct)
  fromProd :: Elems (Rep Struct) -> Rep Struct ()

and we have an instance GPrim (Elems (Rep Struct)).
-}

data Nil = Nil
data xs :* x = xs :* x

class List els where
  type els' :++: els
  append :: els' -> els -> els' :++: els
  unappend :: els' :++: els -> (els', els)

instance List Nil where
  type els' :++: Nil = els'
  append prod Nil = prod
  unappend prod = (prod, Nil)
  {-# INLINE append #-}
  {-# INLINE unappend #-}

instance List xs => List (xs :* x) where
  type els' :++: (xs :* x) = (els' :++: xs) :* x
  append prod (xs :* x) = append prod xs :* x
  unappend (prod :* x) = case unappend prod of (prod', xs) -> (prod', xs :* x)
  {-# INLINE append #-}
  {-# INLINE unappend #-}

-- convert 'Rep' types into types of the form (Nil :* a0 :* a1 :* a2)
class Product a where
  type Elems a
  toProd :: a () -> Elems a
  fromProd :: Elems a -> a ()

-- 'Nil' instance
instance Product U1 where
  type Elems U1 = Nil
  toProd U1 = Nil
  fromProd Nil = U1
  {-# INLINE toProd #-}
  {-# INLINE fromProd #-}

-- drop meta information
instance Product f => Product (M1 i c f) where
  type Elems (M1 i c f) = Elems f
  toProd (M1 f) = toProd f
  fromProd p = M1 (fromProd p)
  {-# INLINE toProd #-}
  {-# INLINE fromProd #-}

-- constants
instance Product (K1 i c) where
  type Elems (K1 i c) = Nil :* c
  toProd (K1 c) = Nil :* c
  fromProd (Nil :* c) = K1 c
  {-# INLINE toProd #-}
  {-# INLINE fromProd #-}

-- products
instance (List (Elems g), Product f, Product g) => Product (f :*: g) where
  type Elems (f :*: g) = Elems f :++: Elems g
  toProd (f :*: g) = append (toProd f) (toProd g)
  fromProd prod = case unappend prod of (fp, gp) -> fromProd fp :*: fromProd gp
  {-# INLINE toProd #-}
  {-# INLINE fromProd #-}

-- | Generic version of Prim
-- This differs from Prim in an important way: the 'gsizeOf#' field does /not/ include
-- any padding at the end of the struct. So, given a product
--
-- ((Nil :* Word8) :* Word32) :* Word8                             (1)
--
-- we would get the following field placement by GPrim:
--
--   <Word8> <3 bytes padding> <Word32> <Word8>     (gsizeOf# == 9, galignment# == 4)
--
-- and note that we have not added padding at the end of the struct to make gsizeOf# a multiple of galignment#.
--
-- The reason for this is that the product (1) may be part of a larger product:
--
-- (((Nil :* Word8) :* Word32) :* Word8) :* Word8
--
-- so we don't want to insert padding prematurely.
--
-- Also different from 'Prim' is that we don't provide setByteArray or setOffAddr, as there
-- is no way to do so using memset. Instead, default implementations are provided in 'Prim'
-- which use writeByteArray and writeOffAddr in a loop.
class GPrim a where
  gsizeOf#    :: a -> Int#
  galignment# :: a -> Int#
  gindexByteArrayA# :: ByteArray# -> Int# -> a
  greadByteArrayA# :: MutableByteArray# s -> Int# -> State# s -> (# State# s, a #)
  gwriteByteArrayA# :: MutableByteArray# s -> Int# -> a -> State# s -> State# s
  gindexOffAddrA# :: Addr# -> Int# -> a
  greadOffAddrA# :: Addr# -> Int# -> State# s -> (# State# s, a #)
  gwriteOffAddrA# :: Addr# -> Int# -> a -> State# s -> State# s

instance GPrim Nil where
  gsizeOf# _ = 0#
  galignment# _ = 1#
  gindexByteArrayA# _ _ = Nil
  greadByteArrayA# _ _ s = (# s, Nil #)
  gwriteByteArrayA# _ _ _ s = s
  gindexOffAddrA# _ _ = Nil
  greadOffAddrA# _ _ s = (# s, Nil #)
  gwriteOffAddrA# _ _ _ s = s
  {-# INLINE gsizeOf# #-}
  {-# INLINE galignment# #-}
  {-# INLINE gindexByteArrayA# #-}
  {-# INLINE greadByteArrayA# #-}
  {-# INLINE gwriteByteArrayA# #-}
  {-# INLINE gindexOffAddrA# #-}
  {-# INLINE greadOffAddrA# #-}
  {-# INLINE gwriteOffAddrA# #-}

{-# INLINE helpers #-}
helpers :: forall x xs. (Prim x, GPrim xs)
         => xs :* x
         -> (# Int# -- alignment
             , Int# -- size
             , Int# -> Int# -- indexing function for x
             , Int# -> Int# -- indexing function for xs
             #)
helpers _ =
  -- memory storage:
  --    <xs> <padding> <x>
  -- so <padding> is minimal such that @sizeOf xs + pad@ is divisible by @alignX@
  let
    sizeX# = sizeOf# (undefined :: x)
    sizeXs# = gsizeOf# (undefined :: xs)
    alignX# = alignment# (undefined :: x)
    alignXs# = galignment# (undefined :: xs)
    r# = remInt# sizeXs# alignX#
    pad# = if r# ==# 0# then 0# else alignX# -# r#
    sizeTot# = sizeX# +# pad# +# sizeXs#
    alignBoth# = if alignX# >=# alignXs# then alignX# else alignXs# 
       -- here we assume that alignments all divide each other (i.e. powers of 2)
    ixXs# i# = i# *# (quotInt# alignBoth# alignXs#)
    ixX# i# = (i# *# (quotInt# alignBoth# alignX#)) +# (quotInt# (sizeXs# +# pad#) alignX#)
  in
   (# alignBoth#, sizeTot#, ixX#, ixXs# #)

instance (Prim x, GPrim xs) => GPrim (xs :* x) where
  gsizeOf# _ = case helpers (undefined :: xs :* x) of
    (# _, size#, _, _ #) -> size#
  galignment# _ = case helpers (undefined :: xs :* x) of
    (# align#, _, _, _ #) -> align#
  gindexByteArrayA# arr# i# = case helpers (undefined :: xs :* x) of
    (# _, _, ixX#, ixXs# #) ->
      gindexByteArrayA# arr# (ixXs# i#) :* indexByteArrayA# arr# (ixX# i#) 
  greadByteArrayA# arr# i# s# = case helpers (undefined :: xs :* x) of
    (# _, _, ixX#, ixXs# #) ->
      case greadByteArrayA# arr# (ixXs# i#) s# of
       (# s1#, xs #) -> case readByteArrayA# arr# (ixX# i#) s1# of
         (# s2#, x #) -> (# s2#, xs :* x #)
  gwriteByteArrayA# arr# i# (xs :* x) s# = case helpers (undefined :: xs :* x) of
    (# _, _, ixX#, ixXs# #) ->
      writeByteArrayA# arr# (ixX# i#) x
        (gwriteByteArrayA# arr# (ixXs# i#) xs s#)
  gindexOffAddrA# addr# i# = case helpers (undefined :: xs :* x) of
    (# _, _, ixX#, ixXs# #) ->
     gindexOffAddrA# addr# (ixXs# i#) :* indexOffAddrA# addr# (ixX# i#)
  greadOffAddrA# addr# i# s# = case helpers (undefined :: xs :* x) of
    (# _, _, ixX#, ixXs# #) ->
     case greadOffAddrA# addr# (ixXs# i#) s# of
       (# s1#, xs #) -> case readOffAddrA# addr# (ixX# i#) s1# of
         (# s2#, x #) -> (# s2#, xs :* x #)
  gwriteOffAddrA# addr# i# (xs :* x) s# = case helpers (undefined :: xs :* x) of
    (# _, _, ixX#, ixXs# #) ->
      writeOffAddrA# addr# (ixX# i#) x
        (gwriteOffAddrA# addr# (ixXs# i#) xs s#)
  {-# INLINE gsizeOf# #-}
  {-# INLINE galignment# #-}
  {-# INLINE gindexByteArrayA# #-}
  {-# INLINE greadByteArrayA# #-}
  {-# INLINE gwriteByteArrayA# #-}
  {-# INLINE gindexOffAddrA# #-}
  {-# INLINE greadOffAddrA# #-}
  {-# INLINE gwriteOffAddrA# #-}

instance Prim ()
instance (Prim a, Prim b) => Prim (a, b)
instance (Prim a, Prim b, Prim c) => Prim (a, b, c)
instance (Prim a, Prim b, Prim c, Prim d) => Prim (a, b, c, d)
instance (Prim a, Prim b, Prim c, Prim d, Prim e) => Prim (a, b, c, d, e)
instance (Prim a, Prim b, Prim c, Prim d, Prim e, Prim f) => Prim (a, b, c, d, e, f)
#endif

#define derivePrim(ty, ctr, sz, align, idx_arr, rd_arr, wr_arr, set_arr, idx_addr, rd_addr, wr_addr, set_addr) \
instance Prim ty where {                                        \
  sizeOf# _ = unI# sz                                           \
; alignment# _ = unI# (max sz align)                            \
; indexByteArrayA# arr# i# = ctr (idx_arr arr# i#)               \
; readByteArrayA#  arr# i# s# = case rd_arr arr# i# s# of        \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeByteArrayA# arr# i# (ctr x#) s# = wr_arr arr# i# x# s#    \
; setByteArrayA# arr# i# n# (ctr x#) s#                          \
    = case internal (set_arr arr# i# n# x#) (unsafeCoerce# s#) of \
        { (# s1#, _ #) -> unsafeCoerce# s1# }                   \
                                                                \
; indexOffAddrA# addr# i# = ctr (idx_addr addr# i#)              \
; readOffAddrA#  addr# i# s# = case rd_addr addr# i# s# of       \
                        { (# s1#, x# #) -> (# s1#, ctr x# #) }  \
; writeOffAddrA# addr# i# (ctr x#) s# = wr_addr addr# i# x# s#   \
; setOffAddrA# addr# i# n# (ctr x#) s#                           \
    = case internal (set_addr addr# i# n# x#) (unsafeCoerce# s#) of \
        { (# s1#, _ #) -> unsafeCoerce# s1# }                   \
; {-# INLINE sizeOf# #-}                                        \
; {-# INLINE alignment# #-}                                     \
; {-# INLINE indexByteArrayA# #-}                                \
; {-# INLINE readByteArrayA# #-}                                 \
; {-# INLINE writeByteArrayA# #-}                                \
; {-# INLINE setByteArrayA# #-}                                  \
; {-# INLINE indexOffAddrA# #-}                                  \
; {-# INLINE readOffAddrA# #-}                                   \
; {-# INLINE writeOffAddrA# #-}                                  \
; {-# INLINE setOffAddrA# #-}                                    \
}

unI# :: Int -> Int#
unI# (I# n#) = n#

derivePrim(Word, W#, sIZEOF_WORD, aLIGNMENT_WORD,
           indexWordArray#, readWordArray#, writeWordArray#, setWordArray#,
           indexWordOffAddr#, readWordOffAddr#, writeWordOffAddr#, setWordOffAddr#)
derivePrim(Word8, W8#, sIZEOF_WORD8, aLIGNMENT_WORD8,
           indexWord8Array#, readWord8Array#, writeWord8Array#, setWord8Array#,
           indexWord8OffAddr#, readWord8OffAddr#, writeWord8OffAddr#, setWord8OffAddr#)
derivePrim(Word16, W16#, sIZEOF_WORD16, aLIGNMENT_WORD16,
           indexWord16Array#, readWord16Array#, writeWord16Array#, setWord16Array#,
           indexWord16OffAddr#, readWord16OffAddr#, writeWord16OffAddr#, setWord16OffAddr#)
derivePrim(Word32, W32#, sIZEOF_WORD32, aLIGNMENT_WORD32,
           indexWord32Array#, readWord32Array#, writeWord32Array#, setWord32Array#,
           indexWord32OffAddr#, readWord32OffAddr#, writeWord32OffAddr#, setWord32OffAddr#)
derivePrim(Word64, W64#, sIZEOF_WORD64, aLIGNMENT_WORD64,
           indexWord64Array#, readWord64Array#, writeWord64Array#, setWord64Array#,
           indexWord64OffAddr#, readWord64OffAddr#, writeWord64OffAddr#, setWord64OffAddr#)
derivePrim(Int, I#, sIZEOF_INT, aLIGNMENT_INT,
           indexIntArray#, readIntArray#, writeIntArray#, setIntArray#,
           indexIntOffAddr#, readIntOffAddr#, writeIntOffAddr#, setIntOffAddr#)
derivePrim(Int8, I8#, sIZEOF_INT8, aLIGNMENT_INT8,
           indexInt8Array#, readInt8Array#, writeInt8Array#, setInt8Array#,
           indexInt8OffAddr#, readInt8OffAddr#, writeInt8OffAddr#, setInt8OffAddr#)
derivePrim(Int16, I16#, sIZEOF_INT16, aLIGNMENT_INT16,
           indexInt16Array#, readInt16Array#, writeInt16Array#, setInt16Array#,
           indexInt16OffAddr#, readInt16OffAddr#, writeInt16OffAddr#, setInt16OffAddr#)
derivePrim(Int32, I32#, sIZEOF_INT32, aLIGNMENT_INT32,
           indexInt32Array#, readInt32Array#, writeInt32Array#, setInt32Array#,
           indexInt32OffAddr#, readInt32OffAddr#, writeInt32OffAddr#, setInt32OffAddr#)
derivePrim(Int64, I64#, sIZEOF_INT64, aLIGNMENT_INT64,
           indexInt64Array#, readInt64Array#, writeInt64Array#, setInt64Array#,
           indexInt64OffAddr#, readInt64OffAddr#, writeInt64OffAddr#, setInt64OffAddr#)
derivePrim(Float, F#, sIZEOF_FLOAT, aLIGNMENT_FLOAT,
           indexFloatArray#, readFloatArray#, writeFloatArray#, setFloatArray#,
           indexFloatOffAddr#, readFloatOffAddr#, writeFloatOffAddr#, setFloatOffAddr#)
derivePrim(Double, D#, sIZEOF_DOUBLE, aLIGNMENT_DOUBLE,
           indexDoubleArray#, readDoubleArray#, writeDoubleArray#, setDoubleArray#,
           indexDoubleOffAddr#, readDoubleOffAddr#, writeDoubleOffAddr#, setDoubleOffAddr#)
derivePrim(Char, C#, sIZEOF_CHAR, aLIGNMENT_CHAR,
           indexWideCharArray#, readWideCharArray#, writeWideCharArray#, setWideCharArray#,
           indexWideCharOffAddr#, readWideCharOffAddr#, writeWideCharOffAddr#, setWideCharOffAddr#)
derivePrim(Addr, Addr, sIZEOF_PTR, aLIGNMENT_PTR,
           indexAddrArray#, readAddrArray#, writeAddrArray#, setAddrArray#,
           indexAddrOffAddr#, readAddrOffAddr#, writeAddrOffAddr#, setAddrOffAddr#)

