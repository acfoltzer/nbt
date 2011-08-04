{- |

Module      :  Data.NBT.Tests
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

module Main where

import Data.NBT

import qualified Codec.Compression.GZip as GZip

import qualified Data.ByteString.Lazy as L
import Data.Serialize ( Serialize (..), decode, encode )

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

main = putStrLn "success!"