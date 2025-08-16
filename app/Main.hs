module Main where

import Fermions as F
import Bosons (createBoson, annihilateBoson, makeNBosons)
import Basis (newKetMap, ketQNumber, Ket (..), liftBosonOp, liftFermionOp)
import Fourier (oddKRange, getMomentum, kToMode)
import Control.Monad ((>=>))
import qualified BlockDiagonal as BlockDiag
import Yukawa (buildPotential, getCouplings, freeHamiltonianOp, particleEnergy, buildPositionalNumberOpFourier)
import Utils (Serializable(serialize))
import qualified Config

main :: IO ()
main = run

withDataHeaderFooter :: String -> String -> String
withDataHeaderFooter label s = unlines ["# " <> label, s, "##"]

run :: IO ()
run = do
  result <- Config.load "config.toml"
  let config = case result of
       Left errs -> error errs
       Right x -> x
  let fermionConfig = Config.fermionConfig config
  let bosonConfig = Config.bosonConfig config
  let nF = Config.nModes fermionConfig
  let nB = Config.nModes bosonConfig
  let fermionKs = oddKRange nF
  let bosonKs = oddKRange nB
  let fermions = concatMap (makeNFermions nF) $ Config.getNumberRange fermionConfig
  let bosons = concatMap (makeNBosons nB) $ Config.getNumberRange bosonConfig
  let kets = [Ket f b | f <- fermions, b <- bosons]
  let ketMap = newKetMap (ketQNumber fermionKs bosonKs) kets
  let couplings = getCouplings fermionKs bosonKs
  let potential = buildPotential couplings ketMap
  let len = Config.length config
  let h0 = freeHamiltonianOp fermionEs bosonEs
      fermionEs = map (particleEnergy (Config.mass fermionConfig) . getMomentum len) fermionKs
      bosonEs = map (particleEnergy (Config.mass bosonConfig) . getMomentum len) bosonKs
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
  writeFile (Config.outputPath config) out

