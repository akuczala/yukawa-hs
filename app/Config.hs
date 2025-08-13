{-# LANGUAGE OverloadedStrings #-}

module Config where
import Toml (TomlCodec, double, int, decodeFileEither, TomlDecodeError, prettyTomlDecodeError)
import Toml.Codec ((.=))
import qualified Control.Arrow as Arrow


data Config = Config
  { fermionMass :: Double
  , bosonMass :: Double
  , length :: Double
  , nFermionModes :: Int
  , nBosonModes :: Int
  , minFermionNumber :: Int
  , maxFermionNumber :: Int
  , minBosonNumber :: Int
  , maxBosonNumber :: Int
  }
  deriving Show

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.double "fermionMass" .= fermionMass
  <*> Toml.double "bosonMass" .= bosonMass
  <*> Toml.double "length" .= Config.length
  <*> Toml.int "nFermionModes" .= nFermionModes
  <*> Toml.int "nBosonModes" .= nBosonModes
  <*> Toml.int "minFermionNumber" .= minFermionNumber
  <*> Toml.int "maxFermionNumber" .= maxFermionNumber
  <*> Toml.int "minBosonNumber" .= minBosonNumber
  <*> Toml.int "maxBosonNumber" .= maxBosonNumber

formatErrors :: [TomlDecodeError] -> String
formatErrors errs = unlines $ map (show . Toml.prettyTomlDecodeError) errs

load :: String -> IO (Either String Config)
load fileName = Arrow.left formatErrors <$>Toml.decodeFileEither configCodec fileName