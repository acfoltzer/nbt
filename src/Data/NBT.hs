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

import Data.Serialize ( 
    Serialize (..)
  , getWord8
  , putWord8 
  )
import Data.Serialize.Get ( 
    Get
  , getByteString
  , lookAhead
  , skip
  )
import Data.Serialize.Put ( putByteString )
import Data.Serialize.IEEE754

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8 ( fromString, toString )
import Data.Int ( Int8, Int16, Int32, Int64 )
import Data.Ix ( Ix (rangeSize) )
import Data.Array.IArray ( Array, IArray (bounds) )
import Data.Array.Unboxed ( UArray, listArray, elems )

import Control.Applicative ( (<$>) )
import Control.Monad ( forM_, replicateM )

-- | Tag types listed in order so that deriving 'Enum' will assign
-- them the correct number for the binary type field.
data TagType = EndType
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
               deriving (Show, Eq, Enum)

instance Serialize TagType where
    get = fmap (toEnum . fromIntegral) getWord8
    put = putWord8 . fromIntegral . fromEnum

-- | Primitive representation of NBT data. This type contains both named
-- and unnamed variants; a 'Nothing' name signifies an unnamed tag, so
-- when serialized, neither the name nor the type tag will be put.
data NBT = EndTag
         | ByteTag      (Maybe String) Int8
         | ShortTag     (Maybe String) Int16
         | IntTag       (Maybe String) Int32
         | LongTag      (Maybe String) Int64
         | FloatTag     (Maybe String) Float
         | DoubleTag    (Maybe String) Double
         | ByteArrayTag (Maybe String) (UArray Int32 Int8)
         | StringTag    (Maybe String) Int16 String
         | ListTag      (Maybe String) TagType (Array Int32 NBT)
         | CompoundTag  (Maybe String) [NBT]
         | IntArrayTag  (Maybe String) (UArray Int32 Int32)
           deriving (Show, Eq)

instance Serialize NBT where
  get = get >>= \ty ->
    case ty of
      EndType       -> return EndTag
      ByteType      -> named getByte
      ShortType     -> named getShort
      IntType       -> named getInt
      LongType      -> named getLong
      FloatType     -> named getFloat
      DoubleType    -> named getDouble
      ByteArrayType -> named getByteArray
      StringType    -> named getString
      ListType      -> named getList
      CompoundType  -> named getCompound
      IntArrayType  -> named getIntArray
    where
      -- name combinators
      named f = getName >>= f
      unnamed f = f Nothing
      -- name and payload readers
      getName = do
        strTag <- unnamed getString
        case strTag of
          StringTag Nothing _ str -> return $ Just str
          _ -> error "found tag with unparseable name"
      getByte n   = ByteTag n <$> get
      getShort n  = ShortTag n <$> get
      getInt n    = IntTag n <$> get
      getLong n   = LongTag n <$> get
      getFloat n  = FloatTag n <$> getFloat32be
      getDouble n = DoubleTag n <$> getFloat64be
      getByteArray n = do
        len <- get :: Get Int32
        ByteArrayTag n <$> getArrayElements len
      getString n = do
        len <- get :: Get Int16
        StringTag n len <$> UTF8.toString 
                        <$> getByteString (toEnum $ fromEnum len)
      getList n = do
        ty  <- get :: Get TagType
        len <- get :: Get Int32
        ListTag n ty <$> getListElements len (getListElement ty)
      getListElements len getter = do
        elts <- replicateM (fromIntegral len) getter
        return $ listArray (0, len - 1) elts
      getListElement ty = 
        case ty of
          EndType       -> error "TAG_End can't appear in a list"
          ByteType      -> unnamed getByte
          ShortType     -> unnamed getShort
          IntType       -> unnamed getInt
          LongType      -> unnamed getLong
          FloatType     -> unnamed getFloat
          DoubleType    -> unnamed getDouble
          ByteArrayType -> unnamed getByteArray
          StringType    -> unnamed getString
          ListType      -> unnamed getList
          CompoundType  -> unnamed getCompound
          IntArrayType  -> unnamed getIntArray
      getCompound n = CompoundTag n <$> getCompoundElements
      getCompoundElements = do
        ty <- lookAhead get
        case ty of
          -- if we see an end tag, drop it and end the list
          EndType -> skip 1 >> return []
          -- otherwise keep reading
          _ -> get >>= \tag -> (tag :) <$> getCompoundElements
      getIntArray n = do
        len <- get :: Get Int32
        IntArrayTag n <$> getArrayElements len
      getArrayElements len = do
        elts <- replicateM (fromIntegral len) get
        return $ listArray (0, len - 1) elts
  put tag = 
    case tag of     
      -- named cases      
      EndTag -> put EndType
      ByteTag (Just n) b -> 
        put ByteType >> putName n >> putByte b
      ShortTag (Just n) s -> 
        put ShortType >> putName n >> putShort s
      IntTag (Just n) i -> 
        put IntType >> putName n >> putInt i
      LongTag (Just n) l ->
        put LongType >> putName n >> putLong l
      FloatTag (Just n) f ->
        put FloatType >> putName n >> putFloat f
      DoubleTag (Just n) d ->
        put DoubleType >> putName n >> putDouble d
      ByteArrayTag (Just n) bs ->
        put ByteArrayType >> putName n >> putByteArray bs
      StringTag (Just n) _len str ->
        put StringType >> putName n >> putString str
      ListTag (Just n) ty ts ->
        put ListType >> putName n >> putList ty ts
      CompoundTag (Just n) ts ->
        put CompoundType >> putName n >> putCompound ts
      IntArrayTag (Just n) is ->
        put IntArrayType >> putName n >> putIntArray is
      -- unnamed cases
      -- EndTag can't be unnamed
      ByteTag Nothing b           -> putByte b
      ShortTag Nothing s          -> putShort s
      IntTag Nothing i            -> putInt i
      LongTag Nothing l           -> putLong l
      FloatTag Nothing f          -> putFloat f
      DoubleTag Nothing d         -> putDouble d
      ByteArrayTag Nothing bs     -> putByteArray bs
      StringTag Nothing _len str  -> putString str
      ListTag Nothing ty ts       -> putList ty ts
      CompoundTag Nothing ts      -> putCompound ts
      IntArrayTag Nothing is      -> putIntArray is
    where
      -- name and payload helpers
      putName             = putString
      putByte             = put
      putShort            = put
      putInt              = put
      putLong             = put
      putFloat            = putFloat32be
      putDouble           = putFloat64be
      putByteArray bs     = put (int32ArraySize bs) >> mapM_ put (elems bs)
      putString str       = let bs = UTF8.fromString str 
                                len = fromIntegral (B.length bs)
                            in put (len :: Int16) >> putByteString bs
      putList ty ts       = put ty >> put (int32ArraySize ts) >> mapM_ put (elems ts)
      putCompound ts      = forM_ ts put >> put EndTag
      putIntArray is      = put (int32ArraySize is) >> mapM_ put (elems is)

      int32ArraySize :: (IArray a e) => a Int32 e -> Int32
      int32ArraySize = fromIntegral . rangeSize . bounds
