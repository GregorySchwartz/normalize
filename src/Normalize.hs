{- Normalize
Gregory W. Schwartz

Collections the functions pertaining to the normalization of biological
data, where the rows are entities (genes or proteins) while the columns are
samples.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Normalize
    ( logTransform
    , normalize
    , normalizeBySample
    ) where

-- Standard
import Data.Ord
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Function (on)

-- Cabal
import Control.Lens
import Statistics.Quantile
import qualified Data.Sparse.Common as S
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Statistics.Sample as Stat

-- Local
import Types
import Utility

-- | Log transform the normalize map.
logTransform :: Base -> Map.Map Sample (V.Vector Entity) -> Map.Map Sample (V.Vector Entity)
logTransform (Base base) = (fmap . fmap) (over value (logBase 2))

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
normalize UpperQuartile = Map.map upperQuartileNormalize
normalize None          = id
normalize _             = error "Method not supported by normalize."

-- | Normalize all samples by a specific method using a sparse matrix.
normalizeSparse :: Method -> S.SpMatrix Double -> S.SpMatrix Double
normalizeSparse method@QuantileMedian  = quantileNormalize method
normalizeSparse method@QuantileAverage = quantileNormalize method
normalizeSparse None            = id
normalizeSparse _ = error "Method not supported by normalizeSparse."

-- | Normalize a sample (1) by another sample (2) by division. The
-- NormSampleString contains the string that differentiates (1) from (2).
-- NormSampleString must be within (2) and must make, upon its removal from (2),
-- (1). For instance, if we want to normalize "normalizeMe" by
-- "normalizeMeByThis", we would set this string to be "ByThis" so the
-- normalized values from "normalizeMe" are divided by the normalized values
-- from "normalizeMeByThis". This string must make the latter become the former,
-- so "By" would not work as it would become "normalizeMeThis".
normalizeBySample :: SynonymFlag
                  -> Maybe EntitySep
                  -> NormSampleString
                  -> Map.Map Sample (V.Vector Entity)
                  -> Map.Map Sample (V.Vector Entity)
normalizeBySample synonymFlag entitySep normSampleString =
    Map.map ( V.fromList
            . concatMap (divideBySample synonymFlag . reverse . sort)
            )
        . Map.map groupDivisors
        . Map.mapKeysWith
            (V.++)
            ( Sample
            . T.replace (unNormSampleString normSampleString) ""
            . unSample
            )
        . Map.mapWithKey (tagDivisors entitySep normSampleString)

-- | Partition divisors and dividends and divide.
groupDivisors :: V.Vector (EntityName, (Divisor, Entity))
              -> V.Vector [(Divisor, Entity)]
groupDivisors = V.fromList
              . fmap (F.toList . snd)
              . Map.toAscList
              . Map.fromListWith (Seq.><)
              . fmap (over _2 Seq.singleton)
              . V.toList

-- | The actual subtraction (Z scores) of dividends by divisor. If there are too
-- many divisors, we assume they are a synonym if the SynonymFlag is true, so
-- we only take into account the highest intensity synonym.
divideBySample :: SynonymFlag -> [(Divisor, Entity)] -> [Entity]
divideBySample _ []                                    =
    error $ "Empty division in divideBySample."
divideBySample _ [(Divisor True, _)]                   = []
divideBySample _ ((Divisor False, _):_)                = []
divideBySample (SynonymFlag True) all@((Divisor True, x):(Divisor True, y):_) =
    divideBySample (SynonymFlag False)
        . (: (filter (not . unDivisor . fst) all))
        . maximumBy (comparing (_value . snd))
        . filter (unDivisor . fst)
        $ all
divideBySample (SynonymFlag False) ((Divisor True, x):(Divisor True, y):_) =
    error $ "Too many divisors found including: "
         ++ (show x)
         ++ " and "
         ++ (show y)
divideBySample _ ((Divisor True, x):xs)                                    =
    fmap ((-~) value (_value x) . snd) xs

-- | Tag all divisors in a sample.
tagDivisors :: Maybe EntitySep
            -> NormSampleString
            -> Sample
            -> V.Vector Entity
            -> V.Vector (EntityName, (Divisor, Entity))
tagDivisors entitySep needle haystack =
    fmap (tagDivisor entitySep needle haystack)

-- | Tag divisor in a sample.
tagDivisor :: Maybe EntitySep
           -> NormSampleString
           -> Sample
           -> Entity
           -> (EntityName, (Divisor, Entity))
tagDivisor sep (NormSampleString needle) (Sample haystack) !e =
    ( entityName sep
    , ( Divisor . T.isInfixOf needle $ haystack
      , over sample (T.replace needle "") e
      )
    )
  where
    entityName :: (Maybe EntitySep) -> EntityName
    entityName Nothing              = EntityName . _entity $ e
    entityName (Just (EntitySep s)) =
        EntityName . head . T.splitOn s . _entity $ e

-- | Normalize by the upper quartile method, log 2 transformed.
upperQuartileNormalize :: V.Vector Entity -> V.Vector Entity
upperQuartileNormalize xs =
    fmap (over value (/ uqVal zeroFiltered)) zeroFiltered
  where
    zeroFiltered = V.filter ((> 0) . _value) xs
    uqVal = continuousBy (ContParam 1 1) 3 4 . fmap _value

-- | Quantile normalization for sparse matrices, ignoring zeros.
quantileNormalize :: Method -> S.SpMatrix Double -> S.SpMatrix Double
quantileNormalize method mat =
    fmap (\x -> S.lookupDenseSV (x - 1) summaryVec) rankMat
  where
    summaryFunc QuantileMedian  = medianSparseVector
    summaryFunc QuantileAverage = avgSparseVector
    summaryFunc _ = error "Unsupported method for quantile normalization."
    summaryVec =
        S.sparsifySV . S.vr . fmap (summaryFunc method) . S.toRowsL $ sortMat
    sortMat    = S.fromColsL . fmap sortSparseVector . S.toColsL $ mat
    rankMat    = S.fromColsL . fmap rankSparseVector . S.toColsL $ mat
