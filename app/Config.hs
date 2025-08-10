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
  }
  deriving Show

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.double "fermionMass" .= fermionMass
  <*> Toml.double "bosonMass" .= bosonMass
  <*> Toml.double "length" .= Config.length
  <*> Toml.int "nFermionModes" .= nFermionModes
  <*> Toml.int "nBosonModes" .= nBosonModes

formatErrors :: [TomlDecodeError] -> String
formatErrors errs = unlines $ map (show . Toml.prettyTomlDecodeError) errs

load :: String -> IO (Either String Config)
load fileName = Arrow.left formatErrors <$>Toml.decodeFileEither configCodec fileName