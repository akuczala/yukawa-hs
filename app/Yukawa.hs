module Yukawa where
import BaseTypes (Mass, Momentum, Energy, enumerate, Phase)
import Basis (KetMap, liftBosonOp, Ket, liftFermionOp, ketFermions, ketBosons)
import BlockDiagonal ( BlockDiagonalOperator )
import qualified BlockDiagonal as Block
import Bosons (annihilateBoson, bosonSum)
import Control.Monad ((>=>))
import Fermions (annihilateFermion, createFermion, fermionSum)
import MonoVec (Op, MonoVec (..))
import Control.Lens ((^.))


getCouplings :: [Int] -> [Int] -> [(Int, Int, Int)]
getCouplings fermionKs bosonKs = [(i, j, l) | (i, k1) <- enumerate fermionKs,
  (j, k2) <- enumerate fermionKs,
  (l, q) <- enumerate bosonKs,
  k1 + q == k2
  ]

particleEnergy :: Mass -> Momentum -> Energy
particleEnergy m p = sqrt $ p * p + m * m

freeHamiltonianOp :: [Energy] -> [Energy] -> Op Ket
freeHamiltonianOp fermionEs bosonEs k = MonoVec e k where
  e = fermionSum fermionEs (k ^. ketFermions) + bosonSum bosonEs (k ^. ketBosons)

buildPotential :: (Ord q) => [(Int, Int, Int)] -> KetMap q Ket -> BlockDiagonalOperator q Phase
buildPotential couplings ketMap = foldl1 (Block.+) (map buildBDO couplings) where
  buildBDO (k, l, q) = Block.build op ketMap where
    op = liftBosonOp (annihilateBoson q) >=> liftFermionOp (annihilateFermion k >=> createFermion l)

buildFermionNumber :: (Ord q) => Int -> KetMap q Ket -> BlockDiagonalOperator q Phase
buildFermionNumber nModes ketMap = foldl1 (Block.+) (fmap buildOp [0.. nModes - 1]) where
  buildOp k = Block.build op ketMap where
    op = liftFermionOp $ annihilateFermion k >=> createFermion k