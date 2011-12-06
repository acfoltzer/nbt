{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.NBT

import qualified Codec.Compression.GZip as GZip
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Serialize ( Serialize (..), decode, encode )
import qualified Data.ByteString.UTF8 as UTF8 ( fromString, toString )

import Control.Applicative
import Control.Monad
import Data.Int ( Int32 )

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Test.HUnit

instance Arbitrary TagType where
    arbitrary = toEnum <$> choose (0, 10)

eitherErr = either error id

prop_TagType :: TagType -> Bool
prop_TagType ty = eitherErr (decode (encode ty)) == ty

instance Arbitrary NBT where
  arbitrary = do
    ty <- arbitrary
    name <- arbitrary
    let mkArb ty name = 
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
              ty <- arbitrary `suchThat` (EndType /=)
              len <- (toEnum . fromEnum) <$> choose (0, 10 :: Int) :: Gen Int32
              ts <- replicateM (toEnum $ fromEnum len) (mkArb ty Nothing)
              return $ ListTag name ty len ts
            CompoundType -> do
              n <- choose (0, 10)
              ts <- replicateM n (arbitrary `suchThat` (EndTag /=) :: Gen NBT)
              return $ CompoundTag name ts
    mkArb ty (Just name)

prop_NBTroundTrip :: NBT -> Bool
prop_NBTroundTrip nbt = eitherErr (decode (encode nbt)) == nbt

testWorld = do
  fileL <- GZip.decompress <$> L.readFile "testWorld/level.dat"
  let file = B.pack (L.unpack fileL)
      dec = eitherErr (decode file) :: NBT
      enc = encode dec
  enc @?= file 
  eitherErr (decode enc) @?= dec

tests = [
    testProperty "Tag roundtrip" prop_TagType
  , testProperty "NBT roundtrip" prop_NBTroundTrip
  , testCase "testWorld roundtrip" testWorld
  ]

main = defaultMain tests