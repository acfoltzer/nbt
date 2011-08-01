{- |

Module      :  Data.NBT
Copyright   :  (c) Adam C. Foltzer 2010-2011
License     :  BSD3

Maintainer  :  acfoltzer@gmail.com
Stability   :  experimental
Portability :  portable

Defines a 'Data.Serialize.Serialize' instance for Minecraft's NBT
binary data format. See the NBT specification for details:
<https://raw.github.com/acfoltzer/nbt/master/NBT-spec.txt>.

-}

module Data.NBT where

import Data.Serialize

import qualified Data.ByteString as B
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8)

import Control.Monad

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

-- | Primitive representation of NBT data.
data NBT = EndTag
         | ByteTag String Int8
         | ShortTag String Int16
         | IntTag String Int32
         | LongTag String Int64
         | FloatTag String Float
         | DoubleTag String Double
         | ByteArrayTag String Int32 [Word8]
         | StringTag String Int16 String
         | ListTag String TagType Int32 [NBT]
         | CompoundTag String [NBT]
           deriving (Show, Eq)