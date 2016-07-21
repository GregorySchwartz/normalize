{- Load
Gregory W. Schwartz

Collections the functions pertaining to the loading of data.
-}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Load
    ( csvRowToEntity
    , toSampleMap
    ) where

-- Standard
import Data.Maybe
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

-- Cabal
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Read as T

-- Local
import Types

-- | Convert CSV entries into entities.
csvRowToEntity :: Maybe Field
               -> Field
               -> Field
               -> Field
               -> V.Vector T.Text
               -> Entity
csvRowToEntity labelF (Field sampleF) (Field entityF) (Field valueF) row =
    Entity
        { _label  =
            fromMaybe "" . fmap ((row V.!) . (+ (-1)) . unField) $ labelF
        , _sample = row V.! (sampleF - 1)
        , _entity = row V.! (entityF - 1)
        , _value  = either error fst . T.double $ row V.! (valueF - 1)
        }

-- | Convert entities to a sample map, where each sample contains
-- a collection of entities.
toSampleMap :: V.Vector Entity -> Map.Map Sample (V.Vector Entity)
toSampleMap = Map.fromListWith (V.++)
            . V.toList
            . V.map (\ !x -> (Sample $ _sample x, V.singleton x))
