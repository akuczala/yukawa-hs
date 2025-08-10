module Block where
import Data.Map (Map)
import Utils (zipMapsWith)
import MonoVec (Op, MonoVec (..))
import Basis (KetMap (..))
import BaseTypes (Phase)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

newtype BlockOperator q a = BlockOperator (Map (q, q) (Map (Int, Int) a))
  deriving Functor

zipWith :: Ord q => (c -> c -> c) -> BlockOperator q c -> BlockOperator q c -> BlockOperator q c
zipWith f (BlockOperator ma) (BlockOperator mb) = BlockOperator $ zipMapsWith zipBlocks ma mb where
  zipBlocks = zipMapsWith f

-- TODO: simplify by separating matrix element generation from storing in map. or never store in maps.
build :: (Ord q, Ord k) => Op k -> KetMap q k -> BlockOperator q Phase
build op ketMap = BlockOperator $ Map.foldlWithKey go Map.empty (qnumToStatesToIndex ketMap) where
  go accMap q ketToIndex =  Map.foldlWithKey go2 accMap ketToIndex where
    go2 m ket i = fromMaybe m $ do
      (outQ, j, ph) <- applyOp ket
      return $ Map.insertWith (zipMapsWith (Prelude.+)) (q, outQ) (Map.singleton (i, j) ph) m
    applyOp inKet = case op inKet of
      ZeroVec -> Nothing
      MonoVec ph outKet -> do
        j <- Map.lookup outKet ketToIndex
        outQ <- Map.lookup outKet (stateToQNum ketMap)
        Just (outQ, j, ph)