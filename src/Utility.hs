{- Utility
Gregory W. Schwartz

Collections helper functions for the program.
-}

{-# LANGUAGE BangPatterns #-}

module Utility
    ( sortVector
    , rankVector
    , medianVector
    , avgVector
    , transposeSamples
    ) where

-- Standard
import Data.Function (on)
import Data.List

-- Cabal
import Control.Lens
import Statistics.Quantile
import Statistics.Sample (mean)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V

-- Local
import Types

-- | Find the average of a vector.
avgVector :: V.Vector Double -> Double
avgVector = mean

-- | Find the median of a vector.
medianVector :: V.Vector Double -> Double
medianVector = continuousBy s 2 4

-- | Sort a vector of elements.
sortVector :: V.Vector Entity -> V.Vector Entity
sortVector = V.modify (V.sortBy (compare `on` view value))

-- | Get the rank transformed vector of elements.
rankVector :: V.Vector Entity -> V.Vector Entity
rankVector xs = V.map (over value (flip (Map.findWithDefault 0) rankMap)) xs
  where
    rankMap = Map.fromList
            . flip zip [0,1..]
            . Set.toList
            . Set.fromList
            . fmap (view value)
            . V.toList
            $ xs

-- | Transpose sample map. Important: assumes same number of entities in each
-- sample.
transposeSamples :: Map.Map Sample (V.Vector Entity)
                 -> [V.Vector Double]
transposeSamples = fmap V.fromList
                 . transpose
                 . fmap (V.toList . fmap (view value))
                 . Map.elems
