{-# LANGUAGE TemplateHaskell #-}

module Basis where
import BaseTypes (enumerate)
import Fermions (fermionSum, countFermions, asList, Fermions)
import Bosons
import Control.Lens.TH ( makeLenses )
import Control.Lens ((^.), Getter)
import Data.Map (Map)
import qualified Data.Map as Map
import MonoVec (Op)
import Data.List (intercalate)
import Dirac (DiracFermions, getBFermions, getDFermions, liftToBModeOp, liftToDModeOp)

-- buildBasisWithLimit :: Count -> ([Count] -> Bool) -> [[Mode]]
-- buildBasisWithLimit n pred = go initMembers where
--   initMembers = filter pred $ [] : [[mode] | mode <-[0 .. n - 1]]


data Ket = Ket {_ketFermions :: DiracFermions, _ketBosons :: Bosons}
  deriving (Show, Eq, Ord)
makeLenses ''Ket

ketBFermions :: Getter Ket Fermions
ketBFermions = ketFermions . getBFermions

ketDFermions :: Getter Ket Fermions
ketDFermions = ketFermions . getDFermions

liftBFermionOp :: (Int -> Op Fermions) -> (Int -> Op Ket)
liftBFermionOp opk = ketFermions . liftToBModeOp opk

liftDFermionOp :: (Int -> Op Fermions) -> (Int -> Op Ket)
liftDFermionOp opk = ketFermions . liftToDModeOp opk

liftFermionOp :: Op DiracFermions -> Op Ket
liftFermionOp = ketFermions

liftBosonOp :: Op Bosons -> Op Ket
liftBosonOp = ketBosons

prettyKet :: Int -> Ket -> ([Int], [Int], [(Int, Int)])
prettyKet nf ket = (asList nf (ket ^. ketBFermions), asList nf (ket ^. ketDFermions), bosonToList $ ket ^. ketBosons)

-- todo use ⟩
prettyPrintKet :: Int -> Ket -> String
prettyPrintKet nf ket =  "|" <> fString <> "," <> afString <> "," <> bString <> ">" where
  (fModes, afModes, bModes) = prettyKet nf ket
  fString = unwords (map show fModes)
  afString = unwords (map show afModes)
  bString = unwords (fmap (\(m, i) -> show m <> (if i > 1 then "_" <> show i else "")) bModes)

momentumSum :: [Int] -> [Int] -> Ket -> Int
momentumSum fermionKNumbers bosonKNumbers ket =
  fermionSum fermionKNumbers (ket ^. ketBFermions) + fermionSum fermionKNumbers (ket ^. ketDFermions) + bosonSum bosonKNumbers (ket ^. ketBosons)


data QNumbers = QNumbers {nFermions :: Int, kTot :: Int}
  deriving (Show, Eq, Ord)

ketQNumber :: [Int] -> [Int] -> Ket -> QNumbers
ketQNumber fermionKNumbers bosonKNumbers ket = QNumbers {
  nFermions=countFermions (ket ^. ketBFermions) - countFermions (ket ^. ketDFermions),
  kTot=momentumSum fermionKNumbers bosonKNumbers ket
}

data KetMap q k = KetMap {stateToQNum :: Map k q, qnumToStatesToIndex :: Map q (Map k Int)}

newtype BlockSizes q = BlockSizes (Map q Int)

newKetMap :: (Ord k, Ord q) => (k -> q) -> [k] -> KetMap q k
newKetMap fromKet allKets = let
  kToQ = [(k, fromKet k) | k <- allKets]
  qToKs = foldl go Map.empty kToQ
  go m (k, q) = Map.insertWith (\new old -> head new : old) q [k] m
  qToKsToInt = fmap (\ks -> Map.fromList [(k, i) | (i, k) <- enumerate ks]) qToKs
  in
    KetMap {stateToQNum = Map.fromList kToQ, qnumToStatesToIndex = qToKsToInt}

qnumToStates :: KetMap q k -> Map q [k]
qnumToStates ketMap = fmap (fmap fst . Map.toList) (qnumToStatesToIndex ketMap)

getBlockSizes :: KetMap q k -> BlockSizes q
getBlockSizes ketMap = BlockSizes $ Map.size <$> qnumToStatesToIndex ketMap
