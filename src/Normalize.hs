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
    ( normalize
    , normalizeBySample
    ) where

-- Standard
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import Data.Function (on)

-- Cabal
import qualified Data.Vector as V
import qualified Data.Text as T
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

-- | Normalize a sample (1) by another sample (2) by division. The
-- NormSampleString contains the string that differentiates (1) from (2).
-- NormSampleString must be within (2) and must make, upon its removal from (2),
-- (1). For instance, if we want to normalize "normalizeMe" by
-- "normalizeMeByThis", we would set this string to be "ByThis" so the
-- normalized values from "normalizeMe" are divided by the normalized values
-- from "normalizeMeByThis". This string must make the latter become the former,
-- so "By" would not work as it would become "normalizeMeThis".
normalizeBySample :: Maybe EntitySep
                  -> NormSampleString
                  -> Map.Map Sample (V.Vector Entity)
                  -> Map.Map Sample (V.Vector Entity)
normalizeBySample entitySep normSampleString =
    Map.map (V.fromList . concatMap (divideBySample . reverse . sort))
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

-- | The actual subtraction (Z scores) of dividends by divisor.
divideBySample :: [(Divisor, Entity)] -> [Entity]
divideBySample []       = error $ "Empty division in divideBySample."
divideBySample [(_, x)] =
    error $ "No pair found for: " ++ show x
divideBySample ((Divisor True, x):(Divisor True, y):_) = 
    error $ "Too many divisors found including: "
         ++ (show x)
         ++ " and "
         ++ (show y)
divideBySample ((Divisor True, x):xs) = fmap ((-~) value (_value x) . snd) xs

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
    over
        (_2 . _2 . sample)
        (T.replace needle "")
        (entityName sep, (Divisor . T.isInfixOf needle $ haystack, e))
  where
    entityName :: (Maybe EntitySep) -> EntityName
    entityName Nothing       = EntityName . _entity $ e
    entityName (Just (EntitySep s)) =
        EntityName . head . T.splitOn s . _entity $ e
