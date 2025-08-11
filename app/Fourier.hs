module Fourier where

oddKRange :: Int -> [Int]
oddKRange n = [- (n `div` 2) .. n `div` 2]

getMomentum :: Double -> Int -> Double
getMomentum len = ((2.0 * pi / len) *) . fromIntegral

kToMode :: Int -> Int -> Int
kToMode n k = n `div` 2 + k