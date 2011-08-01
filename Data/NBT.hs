{- |

Module      :  Data.NBT
Copyright   :  (c) Adam C. Foltzer 2010-2011
License     :  BSD3

Maintainer  :  acfoltzer@gmail.com
Stability   :  experimental
Portability :  portable

Defines a 'Data.Binary.Binary' instance for Minecraft's NBT
binary data format. See the NBT specification for details:
<https://raw.github.com/acfoltzer/nbt/master/NBT-spec.txt>.

-}

module Data.NBT where

import Data.Binary ( Binary (..), decode, encode, getWord8, putWord8 )
import Data.Binary.Get ( Get, getLazyByteString, lookAhead, skip )
import Data.Binary.IEEE754

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF8
import Data.Int ( Int16, Int32, Int64 )
import Data.Word ( Word8 )

import Control.Applicative ( (<$>) )
import Control.Monad ( replicateM )

import Test.QuickCheck

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
               deriving (Show, Eq, Enum)

instance Binary TagType where
    get = fmap (toEnum . fromEnum) getWord8
    put = putWord8 . toEnum . fromEnum

instance Arbitrary TagType where
    arbitrary = toEnum <$> choose (0, 10)

prop_TagType :: TagType -> Bool
prop_TagType ty = decode (encode ty) == ty

-- | Primitive representation of NBT data. 
data NBT = EndTag
         | ByteTag      (Maybe String) Word8
         | ShortTag     (Maybe String) Int16
         | IntTag       (Maybe String) Int32
         | LongTag      (Maybe String) Int64
         | FloatTag     (Maybe String) Float
         | DoubleTag    (Maybe String) Double
         | ByteArrayTag (Maybe String) Int32 B.ByteString
         | StringTag    (Maybe String) Int16 String
         | ListTag      (Maybe String) TagType Int32 [NBT]
         | CompoundTag  (Maybe String) [NBT]
           deriving (Show, Eq)

instance Binary NBT where
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
        ByteArrayTag n len <$> getLazyByteString (toEnum $ fromEnum len)
      getString n = do
        len <- get :: Get Int16
        StringTag n len <$> UTF8.toString 
                        <$> getLazyByteString (toEnum $ fromEnum len)
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
      getCompound n = CompoundTag n <$> getCompoundElements
      getCompoundElements = do
        ty <- lookAhead get
        case ty of
          -- if we see an end tag, drop it and end the list
          EndType -> skip 1 >> return []
          -- otherwise keep reading
          _ -> get >>= \tag -> (tag :) <$> getCompoundElements
  put = undefined
