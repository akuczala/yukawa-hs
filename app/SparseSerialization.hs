module SparseSerialization where
import NumpySerialization (NpyFile, listToNumpy, SystemFloat (SystemFloat))
import Data.Map (Map)
import qualified Codec.Archive.Zip as ZA
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import Data.Binary ( encode, Word8 )
import Data.Time.Clock.POSIX (getPOSIXTime)

newtype NpzFile = NpzFile (Map String NpyFile)

writeNpzFile :: String -> NpzFile ->  IO ()
writeNpzFile zipName (NpzFile npzMap) = do
  time <- fmap round getPOSIXTime -- TODO this is in the future for some reason
  let toEntry name npyFile = ZA.toEntry fileName time binaryData where
        fileName = name <> ".npy"
        binaryData = encode npyFile
  let entries = fmap (uncurry toEntry) (Map.toList npzMap)
  BL.writeFile zipName . ZA.fromArchive $ foldr ZA.addEntryToArchive ZA.emptyArchive entries


testNpzFile :: IO ()
testNpzFile = do
  let file1 = listToNumpy [1 :: Word8, 2, 3, 4]
  let file2 = listToNumpy $ map SystemFloat [1.0 :: Float, 2.0, 3.0, 4.0]
  writeNpzFile "test.npz" . NpzFile $ Map.insert "file2" file2 $ Map.singleton "file1" file1