# nbt

[![build status][build]][travis]
[![hackage badge][badge]][hackage]
![Minecraft lambda](https://raw.githubusercontent.com/acfoltzer/nbt/master/img/logo.png)

## Usage

The `nbt` library gives you a data type `NBT` along with types to all the tags in an NBT's substructure. The library works by providing an `Serialize` instance for this `NBT` type as well as all the substructure tags by relation.

For the most basic usage, you'll need to read a NBT file in as a `ByteString`, decompress it, and then feed it in as a strict `ByteString` to `decode`:

```haskell
import qualified  Codec.Compression.GZip as GZip
import qualified  Data.ByteString.Lazy as BL
import            Data.NBT
import            Data.Serialize

main :: IO ()
main = do
    -- Grab a raw lazy ByteString from some NBT file
    compressedRaw <- BL.readFile "level.dat"

    -- NBT files are GZip'd when stored, decompress it and make it strict
    let raw = BL.toStrict $ GZip.decompress compressedRaw

    -- Use the nbt library's Serialize instance to obtain an NBT type,
    -- provided nothing goes wrong!
    let shouldBeNBT = (decode raw :: Either String NBT)

    -- Did we actually just read an NBT file?
    -- If so, print NBT and then write it back out to file.
    -- Otherwise, show the error.
    case shouldBeNBT of
      Right nbt   -> print nbt >> (BL.writeFile "anotherlevel.dat" $ prep nbt)
      Left err    -> putStrLn err
  where prep n = GZip.compress $ BL.fromChunks [encode n]
```

This `NBT` type has the following structure: `Text` is used for the name of a
tag while `NbtContents` is used for the tag's payload.

```haskell
data NBT = NBT Text NbtContents deriving (Show,Eq)

data NbtContents
  = ByteTag      Int8
  | ShortTag     Int16
  | IntTag       Int32
  | LongTag      Int64
  | FloatTag     Float
  | DoubleTag    Double
  | ByteArrayTag (UArray Int32 Int8)
  | StringTag    T.Text
  | ListTag      (Array Int32 NbtContents)
  | CompoundTag  [NBT]
  | IntArrayTag  (UArray Int32 Int32)
  | LongArrayTag (UArray Int32 Int64)
  deriving (Show, Eq)
```

[build]: https://travis-ci.org/acfoltzer/nbt.png?branch=master
[travis]: https://travis-ci.org/acfoltzer/nbt
[badge]: https://img.shields.io/hackage/v/nbt.svg
[hackage]: http://hackage.haskell.org/package/nbt
