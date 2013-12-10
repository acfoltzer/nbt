{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Codec.Compression.GZip               as GZip
import           Control.Applicative
import           Control.Monad
import qualified Data.Array.IArray                    as IA
import           Data.Array.Unboxed                   (listArray)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as L
import           Data.Int                             (Int32)
import           Data.NBT
import           Data.Serialize                       (decode, encode)
import qualified Data.Text                            as T
import           Paths_nbt                            (getDataFileName)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
import           Test.QuickCheck

instance Arbitrary TagType where
    arbitrary = toEnum <$> choose (1, 10)
    -- don't arbitrarily pick end type, it has special meaning

eitherErr :: (Either String a -> a)
eitherErr = either error id

prop_TagType :: TagType -> Bool
prop_TagType ty = eitherErr (decode (encode ty)) == ty

instance Arbitrary NBT where
  arbitrary = arbitrary >>= \(ty, nm) -> NBT (T.pack nm) <$> mkArb ty
    where
      mkArb ty =
        case ty of
          EndType -> error "can't make end-type"
          ByteType -> ByteTag <$> arbitrary
          ShortType -> ShortTag <$> arbitrary
          IntType -> IntTag <$> arbitrary
          LongType -> LongTag <$> arbitrary
          FloatType -> FloatTag <$> arbitrary
          DoubleType -> DoubleTag <$> arbitrary
          ByteArrayType -> do
            len <- fromIntegral <$> choose (0, 100 :: Int) :: Gen Int32
            ws <- replicateM (fromIntegral len) arbitrary
            return $ ByteArrayTag . listArray (0, len - 1) $ ws
          StringType -> do
            n <- choose (0, 100) :: Gen Int
            str <- T.pack <$> replicateM (fromIntegral n) arbitrary
            return $ StringTag str
          ListType -> do
            subTy <- arbitrary
            len <- fromIntegral <$> choose (0, 11 :: Int) :: Gen Int32
            ts <- replicateM (fromIntegral len) (mkArb subTy)
            return $ ListTag subTy . IA.listArray (0, len - 1) $ ts
          CompoundType -> do
            n <- choose (0, 11)
            ts <- replicateM n arbitrary
            return $ CompoundTag ts
          IntArrayType -> do
            len <- fromIntegral <$> choose (0, 100 :: Int) :: Gen Int32
            IntArrayTag
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
