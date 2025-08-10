module Fermions where

import BaseTypes(Mode, Parity(..), toSign, Size, enumerate)
import MonoVec(MonoVec(..), Op)

import Data.Bits (Bits(popCount, shiftL, complement, (.&.)), testBit)
import Control.Monad ((>=>))
import GHC.Bits (setBit, Bits ((.|.), xor))
import Utils (Serializable (..))


newtype Fermions = Fermions Int
  deriving (Show, Eq, Ord)

allFermions :: Size -> [Fermions]
allFermions n = fmap Fermions [0 .. 2 ^ n - 1]

fermionVacuum :: Fermions
fermionVacuum = Fermions 0

combine :: Fermions -> Fermions -> Fermions
combine (Fermions x) (Fermions y) = Fermions $ xor x y

fermionOccupation :: Mode -> Fermions -> Bool
fermionOccupation mode (Fermions f) = testBit f mode

countFermions :: Fermions -> Size
countFermions (Fermions f) = popCount f

fermionParity :: Fermions -> Parity
fermionParity f = if even (countFermions f) then EvenParity else OddParity

annihilateFermion :: Mode -> Op Fermions
annihilateFermion mode (Fermions f) = if fermionOccupation mode (Fermions f) then
  let
    mask = (1 `shiftL` mode) - 1
    new = Fermions $ f .&. complement (1 `shiftL` mode)
    parity = fermionParity (Fermions (f .&. mask))
    in
    MonoVec (fromIntegral $ toSign parity) new
  else
    ZeroVec
  
createFermion :: Mode -> Op Fermions
createFermion mode (Fermions f) = if not (fermionOccupation mode (Fermions f)) then
  let
    mask = (1 `shiftL` mode) - 1
    new = Fermions $ f .|. (1 `shiftL` mode)
    parity = fermionParity (Fermions (f .&. mask))
    in
    MonoVec (fromIntegral $ toSign parity) new
  else
    ZeroVec

fermionNumber :: Mode -> Op Fermions
fermionNumber mode = annihilateFermion mode >=> createFermion mode


singleFermion :: Mode -> Fermions
singleFermion mode = Fermions $ setBit 0 mode

asList :: Size -> Fermions -> [Mode]
asList size f = [ mode | mode <- [0 .. size - 1], fermionOccupation mode f ]

fermionSum :: Num a => [a] -> Fermions -> a
fermionSum as f = sum $ [a | (k, a) <- enumerate as, fermionOccupation k f] 

instance Serializable Fermions where
  serialize (Fermions f) = show f