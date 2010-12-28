module NBT where

import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8)
import Data.Binary
import Data.Binary.IEEE754
import qualified Codec.Binary.UTF8.String as BStr

import Control.Monad

type TagType = Word8
endType :: TagType 
byteType :: TagType
shortType :: TagType           
intType :: TagType
longType :: TagType          
floatType :: TagType
doubleType :: TagType
byteArrayType :: TagType             
stringType :: TagType
listType :: TagType
compoundType :: TagType
endType = 0
byteType = 1
shortType = 2
intType = 3
longType = 4
floatType = 5
doubleType = 6
byteArrayType = 7
stringType = 8
listType = 9
compoundType = 10

data Tag = EndTag
         | ByteTag String Int8
         | ShortTag String Int16
         | IntTag String Int32
         | LongTag String Int64
         | FloatTag String Float
         | DoubleTag String Double
         | ByteArrayTag String Int32 [Word8]
         | StringTag String Int16 String
         | ListTag String TagType Int32 [Tag]
         | CompoundTag String [Tag]
         deriving (Show, Eq)

instance Binary Tag where
  get = do tagType <- getWord8           
           case tagType of
             0 -> getEnd
             1 ->
               do name <- readName
                  getByte name
             2 ->
               do name <- readName
                  getShort name
             3 ->
               do name <- readName
                  getInt name
             4 ->
               do name <- readName
                  getLong name
             5 ->
               do name <- readName
                  getFloat name
             6 ->
               do name <- readName
                  getDouble name
             7 ->
               do name <- readName
                  getByteArray name
             8 ->
               do name <- readName
                  getString name
             9 ->
               do name <- readName
                  listType <- getWord8
                  length <- get :: Get Int32
                  payload <- getList listType ((fromIntegral . toInteger) length)
                  return $ ListTag name listType length payload
             10 ->
               do name <- readName
                  payload <- getCompound
                  return $ CompoundTag name payload
             _ -> error $ "saw type " ++ (show tagType)
          where readName =
                  do length <- get :: Get Int16
                     words <- sequence $ take ((fromIntegral . toInteger) length)
                                       $ repeat (get :: Get Word8)
                     return $ BStr.decode words
                getEnd = return EndTag
                getByte name = do
                  payload <- get 
                  return $ ByteTag name payload
                getShort name = do
                  payload <- get 
                  return $ ShortTag name payload
                getInt name = do
                  payload <- get
                  return $ IntTag name payload
                getLong name = do
                  payload <- get
                  return $ LongTag name payload
                getFloat name = do
                  payload <- getFloat32be
                  return $ FloatTag name payload
                getDouble name = do
                  payload <- getFloat64be
                  return $ DoubleTag name payload
                getByteArray name = do
                  length <- get :: Get Int32
                  payload <- sequence $ take ((fromIntegral . toInteger) length)
                                      $ repeat (get :: Get Word8)
                  return $ ByteArrayTag name length payload
                getString name = do 
                  length <- get :: Get Int16
                  payload <- sequence $ take ((fromIntegral . toInteger) length)
                                      $ repeat (get :: Get Word8)
                  return $ StringTag name length (BStr.decode payload)
                getList listType 0 = return []
                getList listType n =
                  do tag <- case listType of
                              1 -> getByte ""
                              2 -> getShort ""
                              3 -> getInt ""
                              4 -> getLong ""
                              5 -> getFloat ""
                              6 -> getDouble ""
                              7 -> getByteArray ""
                              8 -> getString ""
                              9 -> do listType' <- getWord8
                                      length <- get :: Get Int32
                                      payload <- getList listType' ((fromIntegral . toInteger) length)
                                      return $ ListTag "" listType' length payload
                              10 -> do payload <- getCompound
                                       return $ CompoundTag "" payload
                     tags <- getList listType (n-1)
                     return (tag:tags)
                getCompound =
                  do tag <- get :: Get Tag
                     case tag of
                       EndTag -> return []
                       _ -> do tags <- getCompound
                               return (tag:tags)
                              


test = do
  file <- BS.readFile "level.dat"
  print $ ((decode $ GZip.decompress file) :: Tag)
