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

import           Control.Applicative    ((<$>))
import           Control.Monad          (forM_, replicateM)
import           Data.Array             (Array, elems, listArray)
import qualified Data.ByteString        as B
import qualified Data.ByteString.UTF8   as UTF8 (fromString, toString)
import           Data.Int               (Int16, Int32, Int64, Int8)
import           Data.Serialize         (Serialize (..), getWord8, putWord8)
import           Data.Serialize.Get     (Get, getByteString, lookAhead, skip)
import           Data.Serialize.IEEE754
import           Data.Serialize.Put     (putByteString)

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
         | ByteArrayTag (Maybe String) Int32 B.ByteString
         | StringTag    (Maybe String) Int16 String
         | ListTag      (Maybe String) TagType Int32 [NBT]
         | CompoundTag  (Maybe String) [NBT]
         | IntArrayTag  (Maybe String) Int32 (Array Int32 Int32)
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
        ByteArrayTag n len <$> getByteString (toEnum $ fromEnum len)
      getString n = do
        len <- get :: Get Int16
        StringTag n len <$> UTF8.toString
                        <$> getByteString (toEnum $ fromEnum len)
      getList n = do
        ty  <- get :: Get TagType
        len <- get :: Get Int32
        ListTag n ty len <$>
          replicateM (toEnum $ fromEnum len) (getListElement ty)
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
        IntArrayTag n len . listArray (0, len-1)
          <$> (replicateM (fromIntegral len) $ get)
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
      ByteArrayTag (Just n) len bs ->
        put ByteArrayType >> putName n >> putByteArray len bs
      StringTag (Just n) _len str ->
        put StringType >> putName n >> putString str
      ListTag (Just n) ty len ts ->
        put ListType >> putName n >> putList ty len ts
      CompoundTag (Just n) ts ->
        put CompoundType >> putName n >> putCompound ts
      IntArrayTag (Just n) len ints ->
        put IntArrayType >> putName n >> putIntArray len ints
      -- unnamed cases
      -- EndTag can't be unnamed
      ByteTag Nothing b            -> putByte b
      ShortTag Nothing s           -> putShort s
      IntTag Nothing i             -> putInt i
      LongTag Nothing l            -> putLong l
      FloatTag Nothing f           -> putFloat f
      DoubleTag Nothing d          -> putDouble d
      ByteArrayTag Nothing len bs  -> putByteArray len bs
      StringTag Nothing _len str   -> putString str
      ListTag Nothing ty len ts    -> putList ty len ts
      CompoundTag Nothing ts       -> putCompound ts
      IntArrayTag Nothing len ints -> putIntArray len ints
    where
      -- name and payload helpers
      putName              = putString
      putByte              = put
      putShort             = put
      putInt               = put
      putLong              = put
      putFloat             = putFloat32be
      putDouble            = putFloat64be
      putByteArray len bs  = put len >> putByteString bs
      putString str        = let bs = UTF8.fromString str
                                 len = fromIntegral (B.length bs)
                             in put (len :: Int16) >> putByteString bs
      putList ty len ts    = put ty >> put len >> forM_ ts put
      putCompound ts       = forM_ ts put >> put EndTag
      putIntArray len ints = put len >> forM_ (elems ints) put
