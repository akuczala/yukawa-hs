module Main where

import Fermions as F
import Bosons (createBoson, annihilateBoson, makeNBosons)
import Basis (newKetMap, ketQNumber, Ket (..), liftBosonOp, liftBFermionOp, liftDFermionOp, getBlockSizes)
import Fourier (oddKRange, getMomentum, kToMode)
import Control.Monad ((>=>))
import qualified BlockDiagonal as BlockDiag
import Yukawa (buildPotential, getCouplings, freeHamiltonianOp, particleEnergy, buildPositionalNumberOpFourier)
import qualified Config
import Config (AntifermionConfig(..))
import Dirac (makeDiracFermions)
import Data.Maybe (fromMaybe, catMaybes)
import Serialization (Serializable(serialize))
import NumpySerialization (testNumpy)

main :: IO ()
main = testNumpy

withDataHeaderFooter :: String -> String -> String
withDataHeaderFooter label s = unlines ["# " <> label, s, "##"]

run :: IO ()
run = do
  result <- Config.load "config.toml"
  let config = case result of
       Left errs -> error errs
       Right x -> x
  let fermionConfig = Config.fermionConfig config
  let antifermionConfig = Config.antifermionConfig config
  let bosonConfig = Config.bosonConfig config
  let nF = Config.nModes fermionConfig
  let nB = Config.nModes bosonConfig
  let fermionKs = oddKRange nF
  let bosonKs = oddKRange nB
  let fermions = concatMap (makeNFermions nF) $ Config.getNumberRange fermionConfig
  let antifermions = concatMap (makeNFermions nF) [afMinNumber antifermionConfig .. afMaxNumber antifermionConfig]
  let bosons = concatMap (makeNBosons nB) $ Config.getNumberRange bosonConfig
  let kets = [Ket (makeDiracFermions nF f af) b| f <- fermions, af <- antifermions, b <- bosons]
  let ketMap = newKetMap (ketQNumber fermionKs bosonKs) kets
  let couplings = getCouplings fermionKs bosonKs
  let potential = buildPotential couplings ketMap
  let len = Config.length config
  let h0 = freeHamiltonianOp fermionEs bosonEs
      fermionEs = map (particleEnergy (Config.mass fermionConfig) . getMomentum len) fermionKs
      bosonEs = map (particleEnergy (Config.mass bosonConfig) . getMomentum len) bosonKs
  let bdagb k l =  liftBFermionOp annihilateFermion (kToMode nF l) >=> liftBFermionOp createFermion (kToMode nF k)
  let ddagd k l = liftDFermionOp annihilateFermion (kToMode nF l) >=> liftDFermionOp createFermion (kToMode nF k)
  let adaga k l = liftBosonOp (annihilateBoson (kToMode nB l) >=> createBoson (kToMode nB k))
  let fermiPosOps = [buildPositionalNumberOpFourier bdagb fermionKs ketMap k | k <- fermionKs]
  let antifermiPosOps = [buildPositionalNumberOpFourier ddagd fermionKs ketMap k | k <- fermionKs]
  let bosePosOps = [buildPositionalNumberOpFourier adaga bosonKs ketMap k | k <- bosonKs]
  -- print $ Map.elems $ BlockDiag.nElements potential
  let wc = Config.writeConfig config
  let possibleStr field str = if fromMaybe True $ field wc then Just str else Nothing
  let out = unlines . catMaybes $
        [
          Just $ withDataHeaderFooter "blockSizes" (serialize . getBlockSizes $ ketMap)
        ] <>
        [ possibleStr Config.writeKetMap $ withDataHeaderFooter "ketMap" (serialize ketMap)
        , possibleStr Config.writeH0 $ withDataHeaderFooter "H0" (serialize (BlockDiag.build h0 ketMap))
        , possibleStr Config.writeV $ withDataHeaderFooter "V" (serialize potential)
        ] <> [
          possibleStr Config.writeFermiPosOps $ withDataHeaderFooter ("fermiPosOp " <> show k) (serialize op) | (k, op) <- zip fermionKs fermiPosOps
        ] <> [
          possibleStr Config.writeAntifermiPosOps $ withDataHeaderFooter ("antifermiPosOp " <> show k) (serialize op) | (k, op) <- zip fermionKs antifermiPosOps
        ] <> [
          possibleStr Config.writeBosonPosOps $ withDataHeaderFooter ("bosePosOp " <> show k) (serialize op) | (k, op) <- zip bosonKs bosePosOps
        ]
  writeFile (Config.outputPath config) out

