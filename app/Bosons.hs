module Bosons where
import MonoVec (Op, MonoVec (ZeroVec, MonoVec))
import qualified Data.Map as Map
import Data.Map(Map)
import BaseTypes
import Control.Monad ((>=>))
import Utils (zipMapsWith, Serializable (..))




newtype Bosons = Bosons (Map Mode Count)
  deriving Show

instance Eq Bosons where
  (Bosons b1) == (Bosons b2) = Map.assocs b1 == Map.assocs b2

instance Ord Bosons where
  compare (Bosons b1) (Bosons b2) = compare (Map.assocs b1) (Map.assocs b2)
  

bosonVacuum :: Bosons
bosonVacuum = Bosons Map.empty

-- allBosons :: Int -> Int -> Bosons
-- allBosons nMax nModes = scanl _ 

combine :: Bosons -> Bosons -> Bosons
combine (Bosons m1) (Bosons m2) = Bosons $ zipMapsWith (+) m1 m2

bosonOccupation :: Mode -> Bosons -> Count
bosonOccupation mode (Bosons b) = Map.findWithDefault 0 mode b

annihilateBoson :: Mode -> Op Bosons
annihilateBoson mode (Bosons b) = case bosonOccupation mode (Bosons b) of
  0 -> ZeroVec
  n -> MonoVec (sqrt (fromIntegral n)) $ Bosons (case n - 1 of
    0 -> Map.delete mode b
    m -> Map.insert mode m b
    )

createBoson :: Mode -> Op Bosons
createBoson mode (Bosons b) = let n = bosonOccupation mode (Bosons b) in
  MonoVec (sqrt (fromIntegral $ n + 1)) $ Bosons (Map.insert mode (n + 1) b)

createBosonLimited :: Count -> Mode -> Op Bosons
createBosonLimited nMax mode (Bosons b) = case bosonOccupation mode (Bosons b) of
  n | n == nMax -> ZeroVec
  n -> MonoVec (sqrt (fromIntegral $ n + 1)) $ Bosons (Map.insert mode (n + 1) b)

bosonNumber :: Mode -> Op Bosons
bosonNumber mode = annihilateBoson mode >=> createBoson mode

bosonToList :: Bosons -> [(Mode, Count)]
bosonToList (Bosons b) = Map.assocs b

bosonSum :: Num a => [a] -> Bosons -> a
bosonSum as b = sum $ [a * fromIntegral (bosonOccupation k b) | (k, a) <- enumerate as]

singleBoson :: Mode -> Bosons
singleBoson mode = Bosons $ Map.singleton mode 1

instance Serializable Bosons where
  serialize (Bosons m) = show $ Map.toList m