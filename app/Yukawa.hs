module Yukawa where
import BaseTypes (Mass, Momentum, Energy, enumerate, Phase)
import Basis (KetMap, liftBosonOp, Ket, liftFermionOp, ketFermions, ketBosons, ketAntifermions, liftAntifermionOp)
import BlockDiagonal ( BlockDiagonalOperator )
import qualified BlockDiagonal as BlockDiag
import Bosons (annihilateBoson, bosonSum, createBoson)
import Control.Monad ((>=>))
import Fermions (annihilateFermion, createFermion, fermionSum)
import MonoVec (Op, MonoVec (..))
import Control.Lens ((^.))
import qualified Block ((+), build)
import Block (BlockOperator)


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
  e = fermionSum fermionEs (k ^. ketFermions) + fermionSum fermionEs (k ^. ketAntifermions) + bosonSum bosonEs (k ^. ketBosons)

buildPotential :: (Ord q) => [(Int, Int, Int)] -> KetMap q Ket -> BlockDiagonalOperator q Phase
buildPotential couplings ketMap = foldl1 (BlockDiag.+) (map buildBDO couplings) where
  buildBDO (k, l, q) = foldl1 (BlockDiag.+) $ map (`BlockDiag.build` ketMap) ops where
    a k = liftBosonOp $ annihilateBoson k
    at k = liftBosonOp $ createBoson k
    b k = liftFermionOp $ annihilateFermion k
    bt k = liftFermionOp $ createFermion k
    d k = liftAntifermionOp $ annihilateFermion k
    dt k = liftAntifermionOp $ createFermion k
    ops = [ a q >=> b k >=> bt l
          , a q >=> d k >=> dt l
          , a q >=> b l >=> d k
          , at q >=> dt k >=> bt l
          ]

buildPositionalNumberOpFourier :: (Ord q) => (Int -> Int -> Op Ket) -> [Int] -> KetMap q Ket -> Int -> BlockOperator q Phase
buildPositionalNumberOpFourier adagAOp ks ketMap k = foldl1 (Block.+) $ map getTerm convKs where
  convKs = [l | l <- ks, (minimum ks <= k - l) && (k - l <= maximum ks)]
  getTerm l = Block.build (adagAOp (-l) (k - l)) ketMap