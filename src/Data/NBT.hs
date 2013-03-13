{-# OPTIONS_GHC -Wall #-}

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

module Data.NBT where

import Control.Applicative    ((<$>))
import Control.Monad          (forM_, replicateM)
import Data.Array.IArray      (Array, IArray (bounds))
import Data.Array.Unboxed     (UArray, listArray, elems)
import Data.Int               (Int16, Int32, Int64, Int8)
import Data.Ix                (Ix (rangeSize))
import Data.Serialize         (Serialize (..), getWord8, putWord8)
import Data.Serialize.Get     (Get, getByteString, lookAhead, skip)
import Data.Serialize.IEEE754
import Data.Serialize.Put     (Put, putByteString)
import Data.Text.Encoding     (encodeUtf8, decodeUtf8)

import qualified Data.ByteString        as B
import qualified Data.Text              as T

-- | Tag types listed in order so that deriving 'Enum' will assign
-- them the correct number for the binary type field.
data TagType
    = ByteType
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
    deriving (Show, Eq, Enum)

instance Serialize TagType where
    get = fmap (toEnum . pred . fromIntegral) getWord8
    put = putWord8 . fromIntegral . succ . fromEnum

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
    | ListTag      TagType (Array Int32 NbtContents)
    | CompoundTag  [NBT]
    | IntArrayTag  (UArray Int32 Int32)
    deriving (Show, Eq)

getByType :: TagType -> Get NbtContents
getByType ByteType = ByteTag <$> get
getByType ShortType = ShortTag <$> get
getByType IntType = IntTag <$> get
getByType LongType = LongTag <$> get
getByType FloatType = FloatTag <$> getFloat32be
getByType DoubleType = DoubleTag <$> getFloat64be
getByType ByteArrayType = do
    len <- get :: Get Int32
    ByteArrayTag <$> getArrayElements len get
getByType StringType = do
    len <- get :: Get Int16
    StringTag . decodeUtf8 <$> getByteString (fromIntegral len)
getByType ListType = do
    subType <- get :: Get TagType
    len <- get :: Get Int32
    ListTag subType <$> getArrayElements len (getByType subType)
getByType CompoundType = CompoundTag <$> getCompoundElements
  where
    getCompoundElements = do
        ty <- lookAhead (get :: Get Int8)
        if ty == 0
            -- if we see an end tag, drop it and end the list
            then skip 1 >> return []
            -- otherwise keep reading
            else get >>= \tag -> (tag :) <$> getCompoundElements
getByType IntArrayType = do
    len <- get :: Get Int32
    IntArrayTag <$> getArrayElements len get

getArrayElements :: (IArray arr a, Integral len, Ix len)
    => len -> Get a -> Get (arr len a)
getArrayElements len getter = do
    elts <- replicateM (fromIntegral len) getter
    return $ listArray (0, len - 1) elts

putContents :: NbtContents -> Put
putContents tag = case tag of
    ByteTag b           -> put b
    ShortTag s          -> put s
    IntTag i            -> put i
    LongTag l           -> put l
    FloatTag f          -> putFloat32be f
    DoubleTag d         -> putFloat64be d
    ByteArrayTag bs     -> put (int32ArraySize bs) >> mapM_ put (elems bs)
    StringTag str       -> let bs = encodeUtf8 str 
                               len = fromIntegral (B.length bs)
                           in put (len :: Int16) >> putByteString bs
    ListTag ty ts       -> put ty >> put (int32ArraySize ts) >> mapM_ putContents (elems ts)
    CompoundTag ts      -> forM_ ts put >> put (0 :: Int8)
    IntArrayTag is      -> put (int32ArraySize is) >> mapM_ put (elems is)
  where
    int32ArraySize :: (IArray a e) => a Int32 e -> Int32
    int32ArraySize = fromIntegral . rangeSize . bounds

instance Serialize NBT where
    get = do
        ty <- get
        StringTag nm <- getByType StringType
        co <- getByType ty
        return $ NBT nm co
    put (NBT name tag) = do
        put (typeOf tag)
        putContents (StringTag name)
        putContents tag

typeOf :: NbtContents -> TagType
typeOf (ByteTag _)          = ByteType
typeOf (ShortTag _)         = ShortType
typeOf (IntTag _)           = IntType
typeOf (LongTag _)          = LongType
typeOf (FloatTag _)         = FloatType
typeOf (DoubleTag _)        = DoubleType
typeOf (ByteArrayTag _)     = ByteArrayType
typeOf (StringTag _)        = StringType
typeOf (ListTag _ _)        = ListType
typeOf (CompoundTag _)      = CompoundType
typeOf (IntArrayTag _)      = IntArrayType
