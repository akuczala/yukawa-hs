module Block where
import Data.Map (Map)
import Utils (zipMapsWith)
import MonoVec (Op, MonoVec (..))
import Basis (KetMap (..))
import BaseTypes (Phase)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

newtype BlockOperator q a = BlockOperator (Map (q, q) (Map (Int, Int) a))
  deriving (Functor, Show)

zipWith :: Ord q => (c -> c -> c) -> BlockOperator q c -> BlockOperator q c -> BlockOperator q c
zipWith f (BlockOperator ma) (BlockOperator mb) = BlockOperator $ zipMapsWith zipBlocks ma mb where
  zipBlocks = zipMapsWith f

-- TODO: simplify by separating matrix element generation from storing in map. or never store in maps.
build :: (Ord q, Ord k) => Op k -> KetMap q k -> BlockOperator q Phase
build op ketMap = BlockOperator $ Map.foldlWithKey go Map.empty (qnumToStatesToIndex ketMap) where
  go accMap q =  Map.foldlWithKey go2 accMap where
    go2 m ket i = fromMaybe m $ do
      (outQ, j, ph) <- applyOp ket
      return $ Map.insertWith (zipMapsWith (Prelude.+)) (outQ, q) (Map.singleton (j, i) ph) m
    applyOp inKet = case op inKet of
      ZeroVec -> Nothing
      MonoVec ph outKet -> do
        outQ <- Map.lookup outKet (stateToQNum ketMap)
        outKetToIndex <- Map.lookup outQ (qnumToStatesToIndex ketMap)
        j <- Map.lookup outKet outKetToIndex
        Just (outQ, j, ph)

(+) :: (Ord q, Num a) => BlockOperator q a -> BlockOperator q a -> BlockOperator q a
(+) = Block.zipWith (Prelude.+)

