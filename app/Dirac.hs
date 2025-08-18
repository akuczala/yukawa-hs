module Dirac where
import Fermions (Fermions (..), firstNModes, lastNModes, firstNFermions, lastNFermions, concatFermions)
import Control.Lens (Lens', lens, Getter, to)
import BaseTypes (Size)
import MonoVec (Op)

data DiracFermions = DiracFermions Size Fermions
  deriving (Show, Eq, Ord)

makeDiracFermions :: Size -> Fermions -> Fermions -> DiracFermions
makeDiracFermions nf bf df = DiracFermions nf $ concatFermions nf bf df

allFermions :: Lens' DiracFermions Fermions
allFermions = lens get_ set_ where
  get_ (DiracFermions _ f) = f
  set_ (DiracFermions nf _) = DiracFermions nf

-- TODO rename liftToB/DModeOp
liftToBModeOp :: (Int -> Op Fermions) -> (Int -> Op DiracFermions)
liftToBModeOp op = liftedOp where
  liftedOp l (DiracFermions nf f) = DiracFermions nf <$> op (firstNModes l) f

liftToDModeOp :: (Int -> Op Fermions) -> (Int -> Op DiracFermions)
liftToDModeOp op = liftedOp where
  liftedOp l (DiracFermions nf f) = DiracFermions nf <$> op (lastNModes nf l) f

getBFermions :: Getter DiracFermions Fermions
getBFermions = to get_ where
  get_ (DiracFermions nf f) = lastNFermions nf f

getDFermions :: Getter DiracFermions Fermions
getDFermions = to get_ where
  get_ (DiracFermions nf f) = firstNFermions nf f