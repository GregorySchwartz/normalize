{- Filter
Gregory W. Schwartz

Collections the functions pertaining to the filtering of low expression entities.
-}

{-# LANGUAGE BangPatterns #-}

module Filter
    ( filterEntitiesBy
    ) where

-- Standard
import Data.Bool
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Function (on)

-- Cabal
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Lens
import qualified Statistics.Sample as Stat

-- Local
import Types

-- | Filter out entities that have less than the specified threshold.
filterValue :: ValueThreshold -> [Entity] -> [Entity]
filterValue (ValueThreshold x) = filter ((>= x) . _value)

-- | Do these values have a standard deviation less than the specified threshold?
stdDevP :: StdDevThreshold -> [Entity] -> Bool
stdDevP (StdDevThreshold x) = (>= x) . Stat.stdDev . V.fromList . fmap _value

-- | Filter out entities that appear less than the specified amount and record
-- the remaining entities' weight.
getViableEntities
    :: Maybe ValueThreshold
    -> Maybe StdDevThreshold
    -> NumSamples
    -> [Entity]
    -> [Entity]
getViableEntities vt st (NumSamples n) =
    concatMap (\xs -> fmap (set numSamples . length $ xs) xs)
        . filter ((>= n) . length . maybe id filterValue vt)
        . groupBy ((==) `on` _entity)
        . sortBy (compare `on` _entity)
  where
    pass xs = ((>= n) . length . maybe id filterValue vt $ xs)
           && (maybe True (flip stdDevP xs) st)

-- | Filter out entities that appear less than the specified amount and record
-- their weight.
filterEntitiesBy
    :: Maybe ValueThreshold
    -> Maybe StdDevThreshold
    -> NumSamples
    -> Map.Map Sample (V.Vector Entity)
    -> Map.Map Sample (V.Vector Entity)
filterEntitiesBy valueThresh stdDevThresh numSamples =
    Map.fromList
        . fmap (\xs -> (Sample . _sample . head $ xs, V.fromList xs))
        . groupBy ((==) `on` _sample)
        . sortBy (compare `on` _sample)
        . getViableEntities valueThresh stdDevThresh numSamples
        . concatMap (V.toList . snd)
        . Map.toAscList
