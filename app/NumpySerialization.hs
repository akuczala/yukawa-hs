module NumpySerialization where
import Data.Binary
    ( Binary(..), encodeFile, encode, putWord8, getWord8 )
import Control.Monad (unless, when)
import Data.Int (Int8)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put (putByteString, putWord16le, putLazyByteString)
import qualified Data.ByteString.Char8 as B
import Data.Binary.Get (getByteString, getWord16le, getRemainingLazyByteString)

data NpyHeader = NpyHeader
  { descr     :: String        -- e.g. "<f8" (little-endian float64)
  , fortran   :: Bool          -- 'fortran_order': False
  , shape     :: [Int]         -- array shape, e.g. [3, 4]
  } deriving (Eq, Show)

data NpyFile = NpyFile
  { header  :: NpyHeader
  , payload :: BL.ByteString  -- raw array data
  } deriving (Eq, Show)

mkHeaderDict :: NpyHeader -> String
mkHeaderDict (NpyHeader descr fortran shape) =
  "{'descr': '" ++ descr ++
  "', 'fortran_order': " ++ showBool fortran ++
  ", 'shape': " ++ showShape shape ++ ", }"
  where
    showBool True  = "True"
    showBool False = "False"
    showShape []   = "()"
    showShape [n]  = "(" ++ show n ++ ",)"
    showShape ns   = "(" ++ concatMap (\n -> show n ++ ",") ns ++ ")"

instance Binary NpyFile where
  put (NpyFile hdr arr) = do
    -- 1. magic string + version
    putByteString (B.pack "\x93NUMPY")
    putWord8 1  -- major version
    putWord8 0  -- minor version

    -- 2. build header dict
    let dict = mkHeaderDict hdr
        dictPadded = padHeader dict

    -- 3. write header length
    putWord16le (fromIntegral (length dictPadded))

    -- 4. write header itself
    putByteString (B.pack dictPadded)

    -- 5. write array data
    putLazyByteString arr

  get = do
    -- 1. magic
    magic <- getByteString 6
    unless (magic == B.pack "\x93NUMPY") (fail "bad magic")

    major <- getWord8
    minor <- getWord8
    when (major /= 1 || minor /= 0) (fail "unsupported version")

    -- 2. header length
    hlen <- getWord16le

    -- 3. header string
    hdrStr <- getByteString (fromIntegral hlen)

    -- (Parsing header into NpyHeader left as exercise — can use regex or parser)
    let hdr = NpyHeader "<i1" False [3,4]  -- stub

    -- 4. everything else = payload
    NpyFile hdr <$> getRemainingLazyByteString

padHeader :: String -> String
padHeader s =
  --let s'   = if last s == '\n' then s else s ++ "\n"
  let s' = s
      len  = length s'
      -- total header size includes 10 bytes of preamble
      total = 10 + len + 1
      pad  = (64 - (total `mod` 64)) `mod` 64
  in s' ++ replicate pad ' ' ++ "\n"

-- | Encode a list of values into a single ByteString
encodeList :: Binary a => [a] -> BL.ByteString
encodeList xs = BL.concat (map encode xs)

testObj :: NpyFile
testObj = NpyFile {
  header=NpyHeader {
    descr="<i1",
    fortran=False,
    shape=[5]
  },
  payload=encodeList [1 :: Int8, 2, 3, 4, 5]
}

testNumpy :: IO()
testNumpy = encodeFile "test-write-3.npy" testObj
