module Serialization where
import Fermions (Fermions (..))
import Bosons (Bosons (..))
import Basis (QNumbers (nFermions, kTot), Ket, KetMap (qnumToStatesToIndex), BlockSizes (..), ketBFermions, ketDFermions, ketBosons)
import Block (BlockOperator (..))
import BlockDiagonal (BlockDiagonalOperator (..))
import qualified Data.Map as Map
import Data.List (intercalate)
import Control.Lens ((^.))

class Serializable a where
  serialize :: a -> String

instance Serializable a => Serializable (a, a) where
  serialize (a1, a2) = "(" <> serialize a1 <> "," <> serialize a2 <> ")"

instance Serializable Fermions where
  serialize (Fermions f) = show f

instance Serializable Bosons where
  serialize (Bosons m) = show $ Map.toList m

instance Serializable QNumbers where
  serialize q = "Q " <> show (nFermions q) <> " " <> show (kTot q)

instance Serializable Ket where
  serialize k = serialize (k ^. ketBFermions) <> " " <> serialize (k ^. ketDFermions) <> " " <> serialize (k ^. ketBosons)

instance (Serializable q, Serializable k) => Serializable (KetMap q k) where
    serialize ketMap = intercalate "\n" $ map perSector (Map.toList $ qnumToStatesToIndex ketMap) where
      perSector (q, m) = serialize q <> "\n" <> intercalate "\n" (map perKetIndex (Map.toList m))
      perKetIndex (k, i) = show i <> ": " <> serialize k

instance Serializable k => Serializable (BlockSizes k) where
  serialize (BlockSizes sizeMap) = unlines $ map perSector $ Map.toList sizeMap where
    perSector (q, n) = serialize q <> ": " <> show n

instance (Serializable q, Show a) => Serializable (BlockOperator q a) where
  serialize :: BlockOperator q a -> String
  serialize (BlockOperator b) = intercalate "\n" $ map perSector $ Map.toList b where
    perSector (qq, m) = serialize qq <> ": " <> show (map perEntry (Map.toList m))
    perEntry ((i, j), ph) = (i, j, ph)

instance (Serializable q, Show a) => Serializable (BlockDiagonalOperator q a) where
  serialize :: BlockDiagonalOperator q a -> String
  serialize (BlockDiagonalOperator b) = intercalate "\n" $ map perSector $ Map.toList b where
    perSector (q, m) = serialize q <> ": " <> show (map perEntry (Map.toList m))
    perEntry ((i, j), ph) = (i, j, ph)