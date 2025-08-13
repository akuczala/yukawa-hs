module Tests where

import Fermions as F
import Bosons (bosonVacuum, bosonToList, createBoson, annihilateBoson, singleBoson, singleBosons, makeNBosons)
import Basis (newKetMap, ketQNumber, Ket (..), qnumToStates, prettyPrintKet, liftBosonOp, qnumToStatesToIndex)
import Fourier (oddKRange, getMomentum)
import qualified Data.Map as Map
import Control.Monad ((>=>))
import MonoVec (MonoVec(..))
import qualified BlockDiagonal as BlockDiag
import BlockDiagonal(BlockDiagonalOperator(..))
import Yukawa (buildPotential, getCouplings, freeHamiltonianOp, particleEnergy)
import Data.Bifunctor (bimap)
import Utils (Serializable(serialize))
import Data.List (nub)

test6 :: IO ()
test6 = do
  let ks = oddKRange 5
  let k = 1
  print [(-l, k - l) | l <- ks, (minimum ks <= k - l) && (k - l <= maximum ks)]

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
  let fermions = makeNFermions n 2
  let bosons = nub $ bosonVacuum : singleBosons n <> makeNBosons n 2
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
  let fermions = fermionVacuum : singleFermions n <> makeNFermions n 2
  -- let bosons = bosonVacuum : singleBosons n <> nBosons n 2
  let bosons = bosonVacuum : makeNBosons n 2
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
