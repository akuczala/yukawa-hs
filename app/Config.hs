{-# LANGUAGE OverloadedStrings #-}

module Config where
import qualified Toml
import Toml.Codec ((.=))
import qualified Control.Arrow as Arrow
import Toml ( TomlCodec, TomlDecodeError )

data ParticleConfig = ParticleConfig
  { mass :: Double
  , nModes :: Int
  , minNumber :: Int
  , maxNumber :: Int
  }
  deriving Show

getNumberRange :: ParticleConfig -> [Int]
getNumberRange config = [minNumber config .. maxNumber config]

data AntifermionConfig = AntifermionConfig
  { afMinNumber :: Int
  , afMaxNumber :: Int
  }
  deriving Show

data WriteConfig = WriteConfig
  { writeKetMap :: Maybe Bool
  , writeH0 :: Maybe Bool
  , writeV :: Maybe Bool
  , writeFermiPosOps :: Maybe Bool
  , writeAntifermiPosOps :: Maybe Bool
  , writeBosonPosOps :: Maybe Bool
  }
  deriving Show

data Config = Config
  { outputPath :: String
  , length :: Double
  , fermionConfig :: ParticleConfig
  , antifermionConfig :: AntifermionConfig
  , bosonConfig :: ParticleConfig
  , writeConfig :: WriteConfig
  }
  deriving Show

antifermionConfigCodec :: TomlCodec AntifermionConfig
antifermionConfigCodec = AntifermionConfig
  <$> Toml.int "minNumber" .= afMinNumber
  <*> Toml.int "maxNumber" .= afMaxNumber

particleConfigCodec :: TomlCodec ParticleConfig
particleConfigCodec = ParticleConfig
  <$> Toml.double "mass" .= mass
  <*> Toml.int "nModes" .= nModes
  <*> Toml.int "minNumber" .= minNumber
  <*> Toml.int "maxNumber" .= maxNumber

writeConfigCodec :: TomlCodec WriteConfig
writeConfigCodec = WriteConfig
  <$> Toml.dioptional (Toml.bool "ketMap") .= writeKetMap
  <*> Toml.dioptional (Toml.bool "H0") .= writeH0
  <*> Toml.dioptional (Toml.bool "V") .= writeV
  <*> Toml.dioptional (Toml.bool "fermiPosOps") .= writeFermiPosOps
  <*> Toml.dioptional (Toml.bool "antifermiPosOps") .= writeAntifermiPosOps
  <*> Toml.dioptional (Toml.bool "bosonPosOps") .= writeBosonPosOps

configCodec :: TomlCodec Config
configCodec = Config
  <$> Toml.string "outputPath" .= outputPath
  <*> Toml.double "length" .= Config.length
  <*> Toml.table particleConfigCodec "fermions" .= fermionConfig
  <*> Toml.table antifermionConfigCodec "antifermions" .= antifermionConfig
  <*> Toml.table particleConfigCodec "bosons" .= bosonConfig
  <*> Toml.table writeConfigCodec "write" .= writeConfig

formatErrors :: [TomlDecodeError] -> String
formatErrors errs = unlines $ map (show . Toml.prettyTomlDecodeError) errs

load :: String -> IO (Either String Config)
load fileName = Arrow.left formatErrors <$>Toml.decodeFileEither configCodec fileName