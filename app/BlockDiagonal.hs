module BlockDiagonal where

import Data.Map (Map)
import qualified Data.Map as Map
import MonoVec (Op, MonoVec (..))
import Data.Maybe (fromMaybe)
import Basis (KetMap (..))
import BaseTypes (Phase)
import Utils (zipMapsWith, Serializable(..))
import Data.List (intercalate)

newtype BlockDiagonalOperator q a = BlockDiagonalOperator (Map q (Map (Int, Int) a))
  deriving Functor

unwrap :: BlockDiagonalOperator q a -> Map q (Map (Int, Int) a)
unwrap (BlockDiagonalOperator m) = m

zipWith :: Ord q => (c -> c -> c) -> BlockDiagonalOperator q c -> BlockDiagonalOperator q c -> BlockDiagonalOperator q c
zipWith f (BlockDiagonalOperator ma) (BlockDiagonalOperator mb) = BlockDiagonalOperator $ zipMapsWith zipBlocks ma mb where
  zipBlocks = zipMapsWith f

build :: Ord k => Op k -> KetMap q k -> BlockDiagonalOperator q Phase
build op ketMap = BlockDiagonalOperator $ fmap perSector (qnumToStatesToIndex ketMap) where
  perSector ketToIndex = Map.foldlWithKey go Map.empty ketToIndex  where
    go accMap ket i = fromMaybe accMap $ do
      (j, ph) <- applyOp ket
      return $ Map.insertWith (Prelude.+) (j, i) ph accMap
    applyOp inKet = case op inKet of
      ZeroVec -> Nothing
      MonoVec ph outKet -> do
        j <- Map.lookup outKet ketToIndex
        Just (j, ph)

(+) :: (Ord q, Num a) => BlockDiagonalOperator q a -> BlockDiagonalOperator q a -> BlockDiagonalOperator q a
(+) = BlockDiagonal.zipWith (Prelude.+)

(-) :: (Ord q, Num a) => BlockDiagonalOperator q a -> BlockDiagonalOperator q a -> BlockDiagonalOperator q a
(-) = BlockDiagonal.zipWith (Prelude.-)

instance (Serializable q, Show a) => Serializable (BlockDiagonalOperator q a) where
  serialize :: BlockDiagonalOperator q a -> String
  serialize (BlockDiagonalOperator b) = intercalate "\n" $ map perSector $ Map.toList b where
    perSector (q, m) = serialize q <> ": " <> show (map perEntry (Map.toList m))
    perEntry ((i, j), ph) = (i, j, ph)