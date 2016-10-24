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
import Data.Function (on)

-- Cabal
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Csv as CSV
import Control.Lens

-- Local
import Types

-- | Convert CSV entries into entities.
csvRowToEntity :: V.Vector T.Text
               -> Maybe Field
               -> Field
               -> Field
               -> Field
               -> V.Vector T.Text
               -> Entity
csvRowToEntity header labelF sampleF entityF valueF row =
    Entity
        { _label      =
            fromMaybe "" . fmap ((row V.!) . flip fieldIndex header) $ labelF
        , _sample     = row V.! (fieldIndex sampleF header)
        , _entity     = row V.! (fieldIndex entityF header)
        , _value      = (\ x -> either (error . (<> (": " <> show x))) fst
                              . T.double
                              $ x
                        )
                      . (V.!) row
                      . fieldIndex valueF
                      $ header
        , _numSamples = 0
        }

-- | Get the index of a field in the header of a csv file.
fieldIndex :: Field -> V.Vector T.Text -> Int
fieldIndex (Field f) =
    fromMaybe (error ("Column " ++ T.unpack f ++ " not found"))
        . V.findIndex (== f)

-- | Convert entities to a sample map, where each sample contains
-- a collection of entities.
toSampleMap :: V.Vector Entity -> Map.Map Sample (V.Vector Entity)
toSampleMap = Map.fromListWith (V.++)
            . V.toList
            . V.map (\ !x -> (Sample $ _sample x, V.singleton x))
