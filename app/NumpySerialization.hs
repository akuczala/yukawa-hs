module NumpySerialization where
import BlockDiagonal (BlockDiagonalOperator)
import Data.Binary (Binary (..), Word8, Word16, Get, encodeFile, encode)
import Control.Monad (foldM_, liftM4)
import Data.Foldable (traverse_)
import GHC.Base (liftM5)
import Data.Int (Int8)
import qualified Data.ByteString.Lazy as BL

numpyMagic :: String
numpyMagic = "\x93NUMPY"

data FormatDict = FormatDict {
  npyDescr :: String,
  npyFortranOrder :: Bool,
  npyShape :: [Int]
}

data NumpyData a = NumpyData {

  npyMajorVersion :: Word8,
  npyMinorVersion :: Word8,
  npyFormatDict :: FormatDict,
  npyData :: [a]
}

instance Show FormatDict where
  show formatDict = "{" <> concatMap (uncurry showEntry) entries <> "}" where
    entries = [
      ("descr", quote $ npyDescr formatDict),
      ("fortran_order", show . npyFortranOrder $ formatDict),
      ("shape", show . npyShape $ formatDict) -- TODO does this work?
      ]
    showEntry key val = quote key <> ": " <> val <> ", "
    quote x = "'" <> x <> "'"

instance Binary FormatDict where
  put = put . show
  get = undefined -- TODO

instance Binary a => Binary (NumpyData a) where
  put numpyData = do
    put numpyMagic
    put . npyMajorVersion $ numpyData
    put . npyMinorVersion $ numpyData
    put $ length $ show $ npyFormatDict numpyData
    put . npyFormatDict $ numpyData
    putList (npyData numpyData)

  get = do
    --traverse_ (\_ -> (get :: Char)) [1 .. length numpyMagic]
    _ <- (get :: Get Char)
    _ <- (get :: Get Char)
    _ <- (get :: Get Char)
    _ <- (get :: Get Char)
    _ <- (get :: Get Char)
    _ <- (get :: Get Char)
    liftM4 NumpyData get get get get


testNumpySerialize :: IO ()
testNumpySerialize = do
  encodeFile "test-write.npy" $ NumpyData
    {
      npyMajorVersion=2,
      npyMinorVersion=0,
      npyFormatDict=FormatDict{npyDescr="i8", npyFortranOrder=False, npyShape=[5]},
      npyData=[1 :: Int8,2,3,4,5]
    }

testNumpySerialize2 :: IO ()
testNumpySerialize2 = do
  BL.writeFile "test-write-2.npy" $ encode (5 :: Word8)