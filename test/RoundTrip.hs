{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Codec.Compression.GZip               as GZip
import           Control.Applicative
import           Control.Monad
import           Data.Array                           (listArray)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as L
import qualified Data.ByteString.UTF8                 as UTF8 (fromString)
import           Data.Int                             (Int32)
import           Data.NBT
import           Data.Serialize                       (decode, encode)
import           Paths_nbt                            (getDataFileName)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
import           Test.QuickCheck

instance Arbitrary TagType where
    arbitrary = toEnum <$> choose (0, 11)

eitherErr :: (Either String a -> a)
eitherErr = either error id

prop_TagType :: TagType -> Bool
prop_TagType ty = eitherErr (decode (encode ty)) == ty

instance Arbitrary NBT where
  arbitrary = do
    rootType <- arbitrary
    name <- Just <$> arbitrary
    mkArb rootType name
    where
      mkArb ty name =
        case ty of
          EndType -> return EndTag
          ByteType -> ByteTag name <$> arbitrary
          ShortType -> ShortTag name <$> arbitrary
          IntType -> IntTag name <$> arbitrary
          LongType -> LongTag name <$> arbitrary
          FloatType -> FloatTag name <$> arbitrary
          DoubleType -> DoubleTag name <$> arbitrary
          ByteArrayType -> do
            len <- (toEnum . fromEnum) <$> choose (0, 100 :: Int) :: Gen Int32
            ws <- replicateM (toEnum $ fromEnum len) arbitrary
            return $ ByteArrayTag name len $ B.pack ws
          StringType -> do
            n <- choose (0, 100) :: Gen Int
            str <- replicateM (toEnum $ fromEnum n) arbitrary
            let len' = (toEnum . fromEnum) (B.length (UTF8.fromString str))
            return $ StringTag name len' str
          ListType -> do
            subTy <- arbitrary `suchThat` (EndType /=)
            len <- (toEnum . fromEnum) <$> choose (0, 11 :: Int) :: Gen Int32
            ts <- replicateM (toEnum $ fromEnum len) (mkArb subTy Nothing)
            return $ ListTag name subTy len ts
          CompoundType -> do
            n <- choose (0, 11)
            ts <- replicateM n (arbitrary `suchThat` (EndTag /=) :: Gen NBT)
            return $ CompoundTag name ts
          IntArrayType -> do
            len <- (toEnum . fromEnum) <$> choose (0, 100 :: Int) :: Gen Int32
            IntArrayTag name len
              . listArray (0, len-1)
              <$> (vector $ fromIntegral len)

prop_NBTroundTrip :: NBT -> Bool
prop_NBTroundTrip nbt = eitherErr (decode (encode nbt)) == nbt

testWorld :: IO ()
testWorld = do
  fileName <- getDataFileName "test/testWorld/level.dat"
  fileL <- GZip.decompress <$> L.readFile fileName
  let file = B.pack (L.unpack fileL)
      dec = eitherErr (decode file) :: NBT
      enc = encode dec
  enc @?= file
  eitherErr (decode enc) @?= dec

tests :: [Test.Framework.Test]
tests = [
    testProperty "Tag roundtrip" prop_TagType
  , testProperty "NBT roundtrip" prop_NBTroundTrip
  , testCase "testWorld roundtrip" testWorld
  ]

main :: IO ()
main = defaultMain tests
