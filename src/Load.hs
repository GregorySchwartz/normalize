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
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import Data.Monoid
import qualified Data.Foldable as F
import Data.Function (on)

-- Cabal
import Control.Lens
import Control.Monad
import qualified Data.Csv as CSV
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V

-- Local
import Types

-- | Convert CSV entries into entities.
csvRowToEntity :: Maybe Field
               -> Field
               -> Field
               -> Field
               -> Map.Map T.Text T.Text
               -> Entity
csvRowToEntity labelF (Field sampleF) (Field entityF) (Field valueF) row =
    Entity
        { _label      =
            fromMaybe "" . join . fmap (flip Map.lookup row . unField) $ labelF
        , _sample     = getMap "sample" sampleF
        , _entity     = getMap "entity" entityF
        , _value      = (\ x -> either (error . (<> (": " <> show x))) fst
                              . T.double
                              $ x
                        )
                      . getMap "value"
                      $ valueF
        , _numSamples = 0
        }
  where
    getMap :: String -> T.Text -> T.Text
    getMap x f =
        fromMaybe (error ("Cannot find " <> x <> " field. " <> show row))
            . Map.lookup f
            $ row

-- | Convert entities to a sample map, where each sample contains
-- a collection of entities.
toSampleMap :: V.Vector Entity -> Map.Map Sample (V.Vector Entity)
toSampleMap = Map.map (V.fromList . F.toList)
            . Map.fromListWith (Seq.><)
            . V.toList
            . V.map (\ !x -> (Sample $ _sample x, Seq.singleton x))
