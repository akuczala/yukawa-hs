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
import Serialization (Serializable(serialize))
import Data.List (nub)
import Dirac (makeDiracFermions, getBFermions, getDFermions, liftToBModeOp, liftToDModeOp)
import Control.Lens ((^.))

test8 :: IO ()
test8 = do
    let nf = 3
    let f1 = makeDiracFermions nf (Fermions 5) (Fermions 7)
    let f2 = makeDiracFermions nf fermionVacuum (singleFermion 0)
    print f1
    print $ f1 ^. getBFermions
    print $ f1 ^. getDFermions
    print f2
    print $ f2 ^. getBFermions
    print $ f2 ^. getDFermions
    let vac = makeDiracFermions nf fermionVacuum fermionVacuum
    let f3 = mvKet $ liftToBModeOp createFermion 0 vac
    print f3
    print $ f3 ^. getBFermions
    print $ f3 ^. getDFermions
    let f4 = mvKet $ liftToDModeOp createFermion 0 vac
    print f4
    print $ f4 ^. getBFermions
    print $ f4 ^. getDFermions
    let f5 = (liftToDModeOp createFermion 0 >=> liftToBModeOp createFermion 0) vac
    let f6 = (liftToBModeOp createFermion 0 >=> liftToDModeOp createFermion 0) vac
    print f5
    print f6

test7 :: IO ()
test7 = print $ makeNBosons 3 2

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
  let kets = [Ket (makeDiracFermions n f fermionVacuum) b | f <- fermions, b <- bosons]
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
  let makeFermions f = makeDiracFermions n f fermionVacuum
  let kets = [Ket (makeFermions f) b | f <- fermions, b <- bosons]
  let ketMap = newKetMap (ketQNumber fermionKNumbers bosonKNumbers) kets
  --print $ fmap (first $ prettyPrintKet n) . Map.toList <$> qnumToStatesToIndex ketMap
  let targetIndex = 2
  let testOp = liftBosonOp (annihilateBoson targetIndex) >=> liftBosonOp (createBoson targetIndex)
  let testKet = Ket (makeFermions (singleFermion targetIndex)) (singleBoson targetIndex)
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
  let makeFermions f = makeDiracFermions n f fermionVacuum
  print fermionKNumbers
  let kets = [Ket (makeFermions f) b | f <- singleFermions n, b <- singleBosons n ]
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
