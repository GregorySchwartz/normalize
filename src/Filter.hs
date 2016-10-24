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

-- Local
import Types

-- | Filter out entities that appear less than the specified amount and record
-- the remaining entities' weight.
getViableEntities :: Maybe ValueThreshold -> NumSamples -> [Entity] -> [Entity]
getViableEntities vt (NumSamples n) =
    concatMap (\xs -> fmap (set numSamples . length $ xs) xs)
        . filter ((>= n) . length)
        . groupBy ((==) `on` _entity)
        . sortBy (compare `on` _entity)
        . maybe id (\(ValueThreshold x) -> filter ((> x) . abs . _value)) vt

-- | Filter out entities that appear less than the specified amount and record
-- their weight.
filterEntitiesBy
    :: Maybe ValueThreshold
    -> NumSamples
    -> Map.Map Sample (V.Vector Entity)
    -> Map.Map Sample (V.Vector Entity)
filterEntitiesBy valueThreshold numSamples =
    Map.fromList
        . fmap (\xs -> (Sample . _sample . head $ xs, V.fromList xs))
        . groupBy ((==) `on` _sample)
        . sortBy (compare `on` _sample)
        . getViableEntities valueThreshold numSamples
        . concatMap (V.toList . snd)
        . Map.toAscList
