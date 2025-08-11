module Utils where

import Data.Map as Map ( Map )
import qualified Data.Map.Merge.Lazy as Merge

zipMapsWith :: Ord k => (c -> c -> c) -> Map k c -> Map k c -> Map k c
zipMapsWith f = Merge.merge Merge.preserveMissing Merge.preserveMissing (Merge.zipWithMatched (const f))

class Serializable a where
  serialize :: a -> String

instance Serializable a => Serializable (a, a) where
  serialize (a1, a2) = "(" <> serialize a1 <> "," <> serialize a2 <> ")"