{- Normalize
Gregory W. Schwartz

Collections the functions pertaining to the normalization of biological
data, where the rows are entities (genes or proteins) while the columns are
samples.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Normalize
    ( normalize
    , normalizeBySample
    ) where

-- Standard
import Data.List
import qualified Data.Map.Strict as Map
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
normalizeBySample :: NormSampleString
                  -> Map.Map Sample (V.Vector Entity)
                  -> Map.Map Sample (V.Vector Entity)
normalizeBySample normSampleString =
    Map.map (fmap divideBySample)
        . Map.map groupDivisors
        . Map.mapKeysWith
            (V.++)
            ( Sample
            . T.replace (unNormSampleString normSampleString) ""
            . unSample
            )
        . Map.mapWithKey (tagDivisors normSampleString)

-- | Partition divisors and divisees and divide.
groupDivisors :: V.Vector (Divisor, Entity) -> V.Vector [(Divisor, Entity)]
groupDivisors = V.fromList . groupBy ((==) `on` snd) . sort . V.toList

-- | The actual division of divisee by divisor.
divideBySample :: [(Divisor, Entity)] -> Entity
divideBySample [((Divisor True), x), ((Divisor False), y)] = 
    y { _value = _value y / _value x }
divideBySample [((Divisor False), x), ((Divisor True), y)] = 
    y { _value = _value x / _value y }
divideBySample [(_, x)]                                              =
    error $ "No pair found for: " ++ show x
divideBySample (x:_)                                                          =
    error $ "Too many \"pairs\" found including: " ++ (show . snd $ x)

-- | Tag all divisors in a sample.
tagDivisors :: NormSampleString
            -> Sample
            -> V.Vector Entity
            -> V.Vector (Divisor, Entity)
tagDivisors needle haystack = fmap (tagDivisor needle haystack)

-- | Tag divisor in a sample.
tagDivisor :: NormSampleString
           -> Sample
           -> Entity
           -> (Divisor, Entity)
tagDivisor (NormSampleString needle) (Sample haystack) =
    over (_2 . sample) (T.replace needle "")
        . (Divisor . T.isInfixOf needle $ haystack,)
