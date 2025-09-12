module NumpySerialization where
import Data.Binary
    ( Binary(..), encodeFile, encode, putWord8, getWord8 )
import Control.Monad (unless, when)
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put (putByteString, putWord16le, putLazyByteString)
import qualified Data.ByteString.Char8 as B
import Data.Binary.Get (getByteString, getWord16le, getRemainingLazyByteString)
import Data.Word (Word8, Word16)
import GHC.Float (castFloatToWord32, castWord32ToFloat, castDoubleToWord64, castWord64ToDouble)

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

class HasDType a where
  getDtype :: a -> String

instance HasDType Word8 where
  getDtype _ = "<u1"

instance HasDType Word16 where
  getDtype :: Word16 -> String
  getDtype _ = "<u2"

newtype SystemFloat = SystemFloat Float

instance Binary SystemFloat where
  put (SystemFloat x) = put . castFloatToWord32 $ x
  get = SystemFloat . castWord32ToFloat <$> get
  
instance HasDType SystemFloat where
  getDtype _ = ">f4"

newtype SystemDouble = SystemDouble Double

instance Binary SystemDouble where
  put (SystemDouble x) = put . castDoubleToWord64 $ x
  get = SystemDouble . castWord64ToDouble <$> get

instance HasDType SystemDouble where
  getDtype _ = ">f8"

-- list to npy file
listToNumpy :: forall a. (Binary a, HasDType a) => [a] -> NpyFile
listToNumpy as = NpyFile {
  header=NpyHeader {
    descr=getDtype $ head as,
    fortran=False,
    shape=[length as]
  },
  payload=encodeList as
}

testNumpy :: IO()
testNumpy = encodeFile "test-write-3.npy" . listToNumpy . map SystemDouble $ [1.5 :: Double, 1 / 3, 2.0]
