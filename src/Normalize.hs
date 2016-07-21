{- Normalize
Gregory W. Schwartz

Collections the functions pertaining to the normalization of biological
data, where the rows are entities (genes or proteins) while the columns are
samples.
-}

module Normalize
    ( normalize
    ) where

-- Standard
import qualified Data.Map.Strict as Map

-- Cabal
import qualified Data.Vector as V
import qualified Statistics.Sample as Stat
import Control.Lens

-- Local
import Types

-- | Normalize a sample by standard scores.
standardScore :: V.Vector Entity -> V.Vector Entity
standardScore xs = V.map (over value (\x -> (x - mu) / sigma)) xs
  where
    mu    = Stat.mean . V.map _value $ xs
    sigma = Stat.stdDev . V.map _value $ xs

-- | Normalize all samples by a specific method.
normalize :: Method
          -> Map.Map Sample (V.Vector Entity)
          -> Map.Map Sample (V.Vector Entity)
normalize StandardScore = Map.map standardScore
