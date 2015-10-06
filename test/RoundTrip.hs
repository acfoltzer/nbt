{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Codec.Compression.GZip               as GZip
import           Control.Monad
import qualified Data.Array.IArray                    as IA
import           Data.Array.Unboxed                   (listArray)
import qualified Data.ByteString                      as B
import qualified Data.ByteString.Lazy                 as L
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
    arbitrary = toEnum <$> choose (1, 11)
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
          EndType    -> error "can't make end-type"
          ByteType   -> ByteTag   <$> arbitrary
          ShortType  -> ShortTag  <$> arbitrary
          IntType    -> IntTag    <$> arbitrary
          LongType   -> LongTag   <$> arbitrary
          FloatType  -> FloatTag  <$> arbitrary
          DoubleType -> DoubleTag <$> arbitrary
          ByteArrayType -> do
            len  <- choose (0, 100)
            ws   <- replicateM len arbitrary
            let a = listArray (0, fromIntegral len - 1) ws
            return (ByteArrayTag a)
          StringType -> do
            len <- choose (0, 100)
            str <- T.pack <$> replicateM len arbitrary
            return (StringTag str)
          ListType -> do
            len   <- choose (0, 10) -- list types nest, don't get too big
            subTy <- arbitrary
            ts    <- replicateM len (mkArb subTy)
            let a  = IA.listArray (0, fromIntegral len - 1) ts
            return (ListTag a)
          CompoundType -> do
            len <- choose (0, 10) -- compound types nest, don't get too big
            ts  <- replicateM len arbitrary
            return (CompoundTag ts)
          IntArrayType -> do
            len  <- choose (0, 100)
            v    <- vector len
            let a = listArray (0, fromIntegral len - 1) v
            return (IntArrayTag a)

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
