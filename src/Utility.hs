{- Utility
Gregory W. Schwartz

Collections helper functions for the program.
-}

{-# LANGUAGE BangPatterns #-}

module Utility
    ( sortSparseVector
    , rankSparseVector
    , medianSparseVector
    , avgSparseVector
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
import qualified Data.Sparse.Common as S
import qualified Data.Vector.Unboxed as V

-- Local

-- | Find the average of a sparse vector, ignoring zeros.
avgSparseVector :: S.SpVector Double -> Double
avgSparseVector xs = mean . V.fromList . fmap snd . S.toListSV $ xs

-- | Find the median of a sparse vector, ignoring zeros.
medianSparseVector :: S.SpVector Double -> Double
medianSparseVector xs =
    continuousBy s 2 4 . V.fromList . fmap snd . S.toListSV $ xs

-- | Sort a sparse vector, ignoring zeros.
sortSparseVector :: S.SpVector Double -> S.SpVector Double
sortSparseVector xs =
    S.fromListDenseSV (S.svDim xs) . sort . fmap snd . S.toListSV $ xs

-- | Get the rank transformed vector of a sparse vector, ignoring zeros.
rankSparseVector :: S.SpVector Double -> S.SpVector Int
rankSparseVector xs = fmap (\k -> Map.findWithDefault 0 k rankMap) xs
  where
    rankMap = Map.fromList
            . flip zip [1,2..]
            . Set.toList
            . Set.fromList
            . fmap snd
            . S.toListSV
            $ xs
