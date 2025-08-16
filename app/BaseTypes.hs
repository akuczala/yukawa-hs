module BaseTypes where

type Phase = Double

type Mass = Double
type Momentum = Double
type Energy = Double

unitPhase :: Double
unitPhase = 1.0

type Size = Int
type Mode = Int
type Count = Int



data Parity = EvenParity | OddParity
instance Semigroup Parity where
  EvenParity <> EvenParity = EvenParity
  OddParity <> OddParity = EvenParity
  _ <> _ = OddParity

instance Monoid Parity where
  mempty = EvenParity


toSign :: Parity -> Int
toSign EvenParity = 1
toSign OddParity = -1

-- todo: move
enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]
