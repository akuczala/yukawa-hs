module Main where

import Fermions as F
import Bosons (bosonVacuum, bosonToList, createBoson, annihilateBoson, singleBoson)
import Basis (newKetMap, ketQNumber, singleFermions, Ket (..), qnumToStates, singleBosons, prettyPrintKet, liftBosonOp, twoFermions, qnumToStatesToIndex, liftFermionOp, nBosons)
import Fourier (oddKRange, getMomentum, kToMode)
import qualified Data.Map as Map
import Control.Monad ((>=>))
import MonoVec (MonoVec(..))
import qualified Block as Block
import qualified BlockDiagonal as BlockDiag
import BlockDiagonal(BlockDiagonalOperator(..))
import Yukawa (buildPotential, getCouplings, freeHamiltonianOp, particleEnergy, buildPositionalNumberOpFourier)
import Data.Bifunctor (bimap)
import Utils (Serializable(serialize))
import Data.List (nub)
import qualified Config

main :: IO ()
main = run

test6 :: IO ()
test6 = do
  let ks = oddKRange 5
  let k = 1
  print [(-l, k - l) | l <- ks, (minimum ks <= k - l) && (k - l <= maximum ks)]

withDataHeaderFooter :: String -> String -> String
withDataHeaderFooter label s = unlines ["# " <> label, s, "##"]

run :: IO ()
run = do
  result <- Config.load "config.toml"
  let config = case result of
       Left errs -> error errs
       Right x -> x
  let nF = Config.nFermionModes config
  let nB = Config.nBosonModes config
  let fermionKs = oddKRange nF
  let bosonKs = oddKRange nB
  let fermions = singleFermions nF
  let bosons = concatMap (nBosons nB) [0..4]
  let kets = [Ket f b | f <- fermions, b <- bosons]
  let ketMap = newKetMap (ketQNumber fermionKs bosonKs) kets
  let couplings = getCouplings fermionKs bosonKs
  let potential = buildPotential couplings ketMap
  let len = Config.length config
  let h0 = freeHamiltonianOp fermionEs bosonEs
      fermionEs = map (particleEnergy (Config.fermionMass config) . getMomentum len) fermionKs
      bosonEs = map (particleEnergy (Config.bosonMass config) . getMomentum len) bosonKs
  let adagaF k l = liftFermionOp (annihilateFermion (kToMode nF l) >=> createFermion (kToMode nF k))
  let adagaB k l = liftBosonOp (annihilateBoson (kToMode nB l) >=> createBoson (kToMode nB k))
  let fermiPosOps = [buildPositionalNumberOpFourier adagaF fermionKs ketMap k | k <- fermionKs]
  let bosePosOps = [buildPositionalNumberOpFourier adagaB bosonKs ketMap k | k <- bosonKs]
  let out = unlines $ [ withDataHeaderFooter "ketMap" (serialize ketMap)
        , withDataHeaderFooter "H0" (serialize (BlockDiag.build h0 ketMap))
        , withDataHeaderFooter "V" (serialize potential)
        ] <> [
          withDataHeaderFooter ("fermiPosOp " <> show k) (serialize op) | (k, op) <- zip fermionKs fermiPosOps
        ] <> [
          withDataHeaderFooter ("bosePosOp " <> show k) (serialize op) | (k, op) <- zip bosonKs bosePosOps
        ]
  writeFile "out.txt" out


test5 :: IO ()
test5 = do
  let n = 3
  let fermions = allFermions n
  print $ fmap ( bimap (asList 3) (annihilateFermion 0 >=> createFermion 0) . \f -> (f, f)) fermions

test4 :: IO ()
test4 = do
  let n = 3
  let len = 10;
  let fermionMass = 1.0;
  let bosonMass = 0.5;
  let fermionKs = oddKRange n
  let bosonKs = fermionKs
  print fermionKs
  -- let bosons = [bosonVacuum]
  -- let fermions = allFermions n
  let fermions = twoFermions n
  let bosons = nub $ bosonVacuum : singleBosons n <> nBosons n 2
  let kets = [Ket f b | f <- fermions, b <- bosons]
  let ketMap = newKetMap (ketQNumber fermionKs bosonKs) kets
  let couplings = getCouplings fermionKs bosonKs
  -- print couplings
  let potential = buildPotential couplings ketMap
  let h0 = freeHamiltonianOp fermionEs bosonEs
      fermionEs = map (particleEnergy fermionMass . getMomentum len) fermionKs
      bosonEs = map (particleEnergy bosonMass . getMomentum len) bosonKs
  --let blockOp = buildFermionNumber n ketMap
  -- let blockOp = Block.build (liftFermionOp $ annihilateFermion 0 >=> createFermion 0) ketMap
  -- let blockOp2 = Block.build (liftFermionOp $ annihilateFermion 1 >=> createFermion 1) ketMap
  -- let blockOp3 = Block.build (liftFermionOp $ annihilateFermion 2 >=> createFermion 2) ketMap
  -- print $ fmap (second Map.toList) $ Map.toList $ Block.unwrap $ blockOp
  print $ Map.elems <$> Map.elems (qnumToStatesToIndex ketMap)
  putStrLn "ketMap"
  putStrLn $ serialize ketMap
  putStrLn "H0"
  putStrLn $ serialize (BlockDiag.build h0 ketMap)
  putStrLn "\nV"
  putStrLn (serialize potential)

test3 :: IO ()
test3 = do
  let n = 3
  let fermionKNumbers = oddKRange n
  let bosonKNumbers = fermionKNumbers
  print fermionKNumbers
  -- let bosons = [bosonVacuum]
  let fermions = fermionVacuum : singleFermions n <> twoFermions n
  -- let bosons = bosonVacuum : singleBosons n <> nBosons n 2
  let bosons = bosonVacuum : nBosons n 2
  let kets = [Ket f b | f <- fermions, b <- bosons]
  let ketMap = newKetMap (ketQNumber fermionKNumbers bosonKNumbers) kets
  --print $ fmap (first $ prettyPrintKet n) . Map.toList <$> qnumToStatesToIndex ketMap
  let targetIndex = 2
  let testOp = liftBosonOp (annihilateBoson targetIndex) >=> liftBosonOp (createBoson targetIndex)
  let testKet = Ket (singleFermion targetIndex) (singleBoson targetIndex)
  --print $ Map.lookup testKet $ stateToQNum ketMap  
  let testMv = testOp testKet
  let outKet = case testMv of
        MonoVec _ ket -> ket
  putStrLn $ prettyPrintKet n outKet
  --print $ Map.lookup outKet $ stateToQNum ketMap  
  -- print $ map testOp <$> qnumToStates ketMap
  --print $ (\m -> first testOp <$> Map.toList m) <$> qnumToStatesToIndex ketMap
  let dbo = BlockDiag.build testOp ketMap
  print $ Map.toList . (\(BlockDiagonalOperator x) -> x) $ dbo


test2 :: IO ()
test2 = do
  let n = 3
  let fermionKNumbers = oddKRange n
  let bosonKNumbers = fermionKNumbers
  print fermionKNumbers
  let kets = [Ket f b | f <- singleFermions n, b <- singleBosons n ]
  let ketMap = newKetMap (ketQNumber fermionKNumbers bosonKNumbers) kets
  print $ Map.toList $ unwords . map (prettyPrintKet n) <$> qnumToStates ketMap

test1 :: IO ()
test1 = do
  let nf = 5
  let mf = createFermion 1 $ singleFermion 2
  print $ fmap (asList nf) mf
  let mf2 = createFermion 2 $ singleFermion 1
  print $ fmap (asList nf) mf2
  let mb = createBoson 1 bosonVacuum
  print $ fmap bosonToList mb
