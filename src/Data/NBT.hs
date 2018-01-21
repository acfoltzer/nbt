{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

{- |

Module      :  Data.NBT
Copyright   :  (c) Adam C. Foltzer 2010-2011
License     :  BSD3

Maintainer  :  acfoltzer@gmail.com
Stability   :  experimental
Portability :  portable

Defines a Haskell representation of Minecraft's NBT binary data
format, along with instances of 'Data.Serialize.Serialize'. See the
NBT specification for details:
<https://raw.github.com/acfoltzer/nbt/master/NBT-spec.txt>.

-}

module Data.NBT (TagType(..), NBT(..), NbtContents(..), typeOf) where

import Control.Monad          (replicateM)
import Data.Array.IArray      (Array, IArray (bounds))
import Data.Array.Unboxed     (UArray, listArray, elems)
import Data.Foldable          (traverse_)
import Data.Int               (Int16, Int32, Int64, Int8)
import Data.Ix                (Ix (rangeSize))
import Data.Serialize         (Serialize (..), getWord8, putWord8)
import Data.Serialize.Get     (Get, getByteString)
import Data.Serialize.IEEE754
import Data.Serialize.Put     (Put, putByteString)
import Data.Text.Encoding     (encodeUtf8, decodeUtf8)

import qualified Data.ByteString        as B
import qualified Data.Text              as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative    ((<$>),(<*>))
#endif

-- | Tag types listed in order so that deriving 'Enum' will assign
-- them the correct number for the binary type field.
data TagType
    = EndType
    | ByteType
    | ShortType
    | IntType
    | LongType
    | FloatType
    | DoubleType
    | ByteArrayType
    | StringType
    | ListType
    | CompoundType
    | IntArrayType
    | LongArrayType
    deriving (Show, Eq, Enum)

instance Serialize TagType where
    get = fmap (toEnum . fromIntegral) getWord8
    put = putWord8 . fromIntegral . fromEnum

-- | Primitive representation of NBT data. This type contains only the data
-- part, since named nodes can only exist inside compound nodes.
data NBT = NBT T.Text NbtContents
    deriving (Show, Eq)

data NbtContents
    = ByteTag      Int8
    | ShortTag     Int16
    | IntTag       Int32
    | LongTag      Int64
    | FloatTag     Float
    | DoubleTag    Double
    | ByteArrayTag (UArray Int32 Int8)
    | StringTag    T.Text
    | ListTag      (Array Int32 NbtContents)
    | CompoundTag  [NBT]
    | IntArrayTag  (UArray Int32 Int32)
    | LongArrayTag (UArray Int32 Int64)
    deriving (Show, Eq)

getByType :: TagType -> Get NbtContents
getByType EndType       = fail "Can not get end-marker elements"
getByType ByteType      = ByteTag      <$> get
getByType ShortType     = ShortTag     <$> get
getByType IntType       = IntTag       <$> get
getByType LongType      = LongTag      <$> get
getByType FloatType     = FloatTag     <$> getFloat32be
getByType DoubleType    = DoubleTag    <$> getFloat64be
getByType ByteArrayType = ByteArrayTag <$> getArrayElements get
getByType StringType    = StringTag    <$> getString
getByType ListType      = ListTag      <$> getList
getByType CompoundType  = CompoundTag  <$> getCompoundElements
getByType IntArrayType  = IntArrayTag  <$> getArrayElements get
getByType LongArrayType = LongArrayTag <$> getArrayElements get

getList :: Get (Array Int32 NbtContents)
getList = do
    subType <- get
    getArrayElements (getByType subType)

putList :: Array Int32 NbtContents -> Put
putList ts = do
    ty <- case elems ts of
            []  -> return EndType
            x:xs | all (\e -> typeOf e == ty) xs -> return ty
                 | otherwise                     -> fail "Attempted to write heterogeneous list"
              where ty = typeOf x
    put ty
    putArray putContents ts

getCompoundElements :: Get [NBT]
getCompoundElements = do
    ty <- get
    case ty of
      EndType -> return []
      _       -> do x  <- getNBT ty
                    xs <- getCompoundElements
                    return (x:xs)

putCompoundElements :: [NBT] -> Put
putCompoundElements xs = traverse_ put xs >> put EndType

getArrayElements :: IArray a e => Get e -> Get (a Int32 e)
getArrayElements getter = do
    len  <- get
    elts <- replicateM (fromIntegral len) getter
    return (listArray (0, len - 1) elts)

getBytes16 :: Get B.ByteString
getBytes16 = do
    len <- get :: Get Int16
    getByteString (fromIntegral len)

putBytes16 :: B.ByteString -> Put
putBytes16 bs = do
    put (fromIntegral (B.length bs) :: Int16)
    putByteString bs

getString :: Get T.Text
getString = decodeUtf8 <$> getBytes16

putString :: T.Text -> Put
putString = putBytes16 . encodeUtf8

putArray :: (Ix i, IArray a e) => (e -> Put) -> a i e -> Put
putArray putter a = do
    let len = rangeSize (bounds a)
    put (fromIntegral len :: Int32)
    traverse_ putter (elems a)

putContents :: NbtContents -> Put
putContents tag = case tag of
    ByteTag b           -> put b
    ShortTag s          -> put s
    IntTag i            -> put i
    LongTag l           -> put l
    FloatTag f          -> putFloat32be f
    DoubleTag d         -> putFloat64be d
    ByteArrayTag bs     -> putArray put bs
    StringTag str       -> putString str
    ListTag ts          -> putList ts
    CompoundTag ts      -> putCompoundElements ts
    IntArrayTag is      -> putArray put is
    LongArrayTag is     -> putArray put is

instance Serialize NBT where
    get = do
        ty <- get
        getNBT ty
    put (NBT name tag) = do
        put (typeOf tag)
        putString name
        putContents tag

getNBT :: TagType -> Get NBT
getNBT ty = NBT <$> getString <*> getByType ty

typeOf :: NbtContents -> TagType
typeOf ByteTag      {} = ByteType
typeOf ShortTag     {} = ShortType
typeOf IntTag       {} = IntType
typeOf LongTag      {} = LongType
typeOf FloatTag     {} = FloatType
typeOf DoubleTag    {} = DoubleType
typeOf ByteArrayTag {} = ByteArrayType
typeOf StringTag    {} = StringType
typeOf ListTag      {} = ListType
typeOf CompoundTag  {} = CompoundType
typeOf IntArrayTag  {} = IntArrayType
typeOf LongArrayTag {} = LongArrayType
