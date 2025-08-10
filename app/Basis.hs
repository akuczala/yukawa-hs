{-# LANGUAGE TemplateHaskell #-}

module Basis where
import BaseTypes (Size, enumerate)
import Fermions (Fermions, singleFermion, fermionSum, countFermions, asList, combine)
import Bosons
import Control.Lens.TH ( makeLenses )
import Control.Lens ((^.))
import Data.Map (Map)
import qualified Data.Map as Map
import MonoVec (Op)
import qualified Fermions as F
import qualified Bosons as B
import Utils (Serializable (serialize))
import Data.List (intercalate)

-- buildBasisWithLimit :: Count -> ([Count] -> Bool) -> [[Mode]]
-- buildBasisWithLimit n pred = go initMembers where
--   initMembers = filter pred $ [] : [[mode] | mode <-[0 .. n - 1]]

singleFermions :: Size -> [Fermions]
singleFermions nf = map singleFermion [0..nf - 1]

twoFermions :: Size -> [Fermions]
twoFermions n = [F.combine f1 f2 | f1 <- fs, f2 <- fs, f1 < f2] where
  fs = singleFermions n

singleBosons :: Size -> [Bosons]
singleBosons nb = map singleBoson [0..nb - 1]

twoBosons :: Size -> [Bosons]
twoBosons n = [B.combine b1 b2 | b1 <- bs, b2 <- bs] where
  bs = singleBosons n

data Ket = Ket {_ketFermions :: Fermions, _ketBosons :: Bosons}
  deriving (Show, Eq, Ord)
makeLenses ''Ket

instance Serializable Ket where
  serialize k = serialize (k ^. ketFermions) <> " " <> serialize (k ^. ketBosons)

liftFermionOp :: Op Fermions -> Op Ket
liftFermionOp = ketFermions

liftBosonOp :: Op Bosons -> Op Ket
liftBosonOp = ketBosons

prettyKet :: Int -> Ket -> ([Int], [(Int, Int)])
prettyKet nf ket = (asList nf $ ket ^. ketFermions, bosonToList $ ket ^. ketBosons)

-- todo use ⟩
prettyPrintKet :: Int -> Ket -> String
prettyPrintKet nf ket =  "|" <> fString <> "," <> bString <> ">" where
  (fModes, bModes) = prettyKet nf ket
  fString = unwords (map show fModes)
  bString = unwords (fmap (\(m, i) -> show m <> (if i > 1 then "_" <> show i else "")) bModes)

momentumSum :: [Int] -> [Int] -> Ket -> Int
momentumSum fermionKNumbers bosonKNumbers ket =
  fermionSum fermionKNumbers (ket ^. ketFermions) + bosonSum bosonKNumbers (ket ^. ketBosons)


data QNumbers = QNumbers {nFermions :: Int, kTot :: Int}
  deriving (Show, Eq, Ord)

instance Serializable QNumbers where
  serialize q = "Q " <> show (nFermions q) <> " " <> show (kTot q)

ketQNumber :: [Int] -> [Int] -> Ket -> QNumbers
ketQNumber fermionKNumbers bosonKNumbers ket = QNumbers {
  nFermions=countFermions (ket ^. ketFermions),
  kTot=momentumSum fermionKNumbers bosonKNumbers ket
}

data KetMap q k = KetMap {stateToQNum :: Map k q, qnumToStatesToIndex :: Map q (Map k Int)}

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

instance (Serializable q, Serializable k) => Serializable (KetMap q k) where
    serialize ketMap = intercalate "\n" $ map perSector (Map.toList $ qnumToStatesToIndex ketMap) where
      perSector (q, m) = serialize q <> "\n" <> intercalate "\n" (map perKetIndex (Map.toList m))
      perKetIndex (k, i) = show i <> ": " <> serialize k