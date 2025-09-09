module Fermions where

import BaseTypes(Mode, Parity(..), toSign, Size, enumerate)
import MonoVec(MonoVec(..), Op)

import Data.Bits (Bits(popCount, shiftL, shiftR, complement, (.&.)), testBit)
import Control.Monad ((>=>), guard)
import GHC.Bits (setBit, Bits ((.|.), xor))
import Data.Maybe (catMaybes)
import Data.List (nub)


newtype Fermions = Fermions Integer
  deriving (Show, Eq, Ord)

firstNModes :: Size  -> Int
firstNModes i = i

lastNModes :: Size -> Int -> Int
lastNModes n i = n + i

concatFermions :: Size -> Fermions -> Fermions -> Fermions
concatFermions nf (Fermions bf) (Fermions df) = Fermions $ bf + (df `shiftL` nf)

firstNFermions :: Size -> Fermions -> Fermions
firstNFermions n (Fermions f) = Fermions $ f `shiftR` n

lastNFermions :: Size -> Fermions -> Fermions
lastNFermions n (Fermions f) = Fermions $ f .&. (1 `shiftL` n - 1)

allFermions :: Size -> [Fermions]
allFermions n = fmap Fermions [0 .. 2 ^ n - 1]

fermionVacuum :: Fermions
fermionVacuum = Fermions 0

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

singleFermions :: Size -> [Fermions]
singleFermions nf = map singleFermion [0..nf - 1]

combine :: Fermions -> Fermions -> Maybe Fermions
combine (Fermions x) (Fermions y) = do
  guard $ x .&. y == 0
  return . Fermions $ xor x y

nextNFermions :: Size -> [Fermions] -> [Fermions]
nextNFermions n fs = nub $ catMaybes [combine f1 f2 | f1 <- fs, f2 <- singleFermions n]

makeNFermions :: Size -> Int -> [Fermions]
makeNFermions nf n = case n of
  0 -> [fermionVacuum]
  1 -> singleFermions nf
  _ -> nextNFermions nf (makeNFermions nf (n -1))

asList :: Size -> Fermions -> [Mode]
asList size f = [ mode | mode <- [0 .. size - 1], fermionOccupation mode f ]

fermionSum :: Num a => [a] -> Fermions -> a
fermionSum as f = sum $ [a | (k, a) <- enumerate as, fermionOccupation k f] 

