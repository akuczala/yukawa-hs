module MonoVec (MonoVec(..), Op) where

import BaseTypes(Phase, unitPhase)


data MonoVec a = MonoVec {mvPhase :: Phase, mvKet :: a} | ZeroVec
  deriving (Functor, Show)

instance Applicative MonoVec where
  ZeroVec <*> _ = ZeroVec
  _ <*> ZeroVec = ZeroVec
  MonoVec ph1 f <*> MonoVec ph2 x = MonoVec (ph1 * ph2) (f x)
  pure = MonoVec unitPhase

instance Monad MonoVec where
  ZeroVec >>= _ = ZeroVec
  MonoVec ph x >>= f = case f x of
    ZeroVec -> ZeroVec
    MonoVec ph2 y -> MonoVec (ph * ph2) y

type Op a = a -> MonoVec a

scale :: Phase -> MonoVec a -> MonoVec a
scale _ ZeroVec = ZeroVec
scale s (MonoVec ph k) = MonoVec (ph * s) k
