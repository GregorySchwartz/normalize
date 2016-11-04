{- normalize
Gregory W. Schwartz

Blah's the blah in the blah
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Standard
import Data.Maybe
import qualified Data.Map.Strict as Map

-- Cabal
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Text as T
import qualified Data.Csv as CSV
import Options.Generic

-- Local
import Types
import Load
import Normalize
import Filter

-- | Command line arguments
data Options = Options { labelField             :: Maybe T.Text
                                    <?> "The column containing the label for the entry."
                       , sampleField            :: T.Text
                                    <?> "The column containing the sample for the entry."
                       , entityField            :: T.Text
                                    <?> "The column containing the id for the entity in the entry."
                       , valueField             :: T.Text
                                    <?> "The column field containing the value for the entry."
                       , entityDiff             :: Maybe T.Text
                                    <?> "When comparing entities that are the same, ignore the text after this separator. Used for the bySample normalization. For example, if we have a strings ARG29_5 and ARG29_7 that we both want to be divided by another entity in another sample called ARG29, we would set this string to be \"_\""
                       , bySample               :: Maybe T.Text
                                    <?> "Normalize as usual, but at the end use this string to differentiate the sample field from the normalization samples, then divide the matching samples with these samples and renormalize. For instance, if we want to normalize \"normalizeMe\" by \"normalizeMeByThis\", we would set this string to be \"ByThis\" so the normalized values from \"normalizeMe\" are divided by the normalized values from \"normalizeMeByThis\". This string must make the latter become the former, so \"By\" would not work as it would become \"normalizeMeThis\". If there is no divisor, we remove that entity."
                       , bySampleRemoveSynonyms :: Bool
                                    <?> "When normalizing by sample, if the divisor appears multiple times we assume those are synonyms. Here, we would remove the synonym with the smaller intensity. If not set, errors out and provides the synonym name."
                       , method                 :: Maybe String
                                    <?> "([StandardScore] | UpperQuartile | None) The method for standardization of the samples."
                       , filterEntitiesMissing  :: Maybe Int
                                    <?> "([0] | INT) Whether to remove entities that appear less than this many times after normalizing."
                       , filterEntitiesValue    :: Maybe Double
                                    <?> "([Nothing] | Double) Whether to remove entities in filterEntitiesMissing but also counting entities with a value of this or less as missing."
                       , filterEntitiesStdDev :: Maybe Double
                                    <?> "([Nothing] | DOUBLE) Remove entities that have less than this value for their standard deviation among all samples they appear in, after normalization."
                       }
               deriving (Generic, Show)

instance ParseRecord Options

main :: IO ()
main = do
    opts <- getRecord "normalize, Gregory W. Schwartz.\
                      \ Normalizes the data (entities, for instance genes or\
                      \ proteins) by column (samples)."

    -- No header so we can READ the header (ugh).
    csvContents <- fmap
                    (either error id . CSV.decode CSV.NoHeader)
                    CL.getContents

    let synonymFlag  = SynonymFlag . unHelpful . bySampleRemoveSynonyms $ opts
        eSep         = fmap EntitySep . unHelpful . entityDiff $ opts
        sampleDiff   = fmap NormSampleString . unHelpful . bySample $ opts
        filterNumSamples =
            NumSamples . fromMaybe 0 . unHelpful . filterEntitiesMissing $ opts
        filterValue      =
            fmap ValueThreshold . unHelpful . filterEntitiesValue $ opts
        filterStdDev     =
            fmap StdDevThreshold . unHelpful . filterEntitiesStdDev $ opts
        entities     = V.map ( csvRowToEntity
                                (V.head csvContents)
                                (fmap Field . unHelpful $ labelField opts)
                                (Field . unHelpful $ sampleField opts)
                                (Field . unHelpful $ entityField opts)
                                (Field . unHelpful $ valueField opts)
                             )
                        . V.tail
                        $ csvContents
        sampleMap    = toSampleMap entities
        normalizeMap = normalize
                        (maybe StandardScore read . unHelpful . method $ opts)
        result = filterEntitiesBy filterValue filterStdDev filterNumSamples
               . (\ x
                 -> maybe
                        x
                        ( normalizeMap
                        . flip (normalizeBySample synonymFlag eSep) x
                        )
                        sampleDiff
                 )
               . normalizeMap
               $ sampleMap
        formatted = CL.append (CL.pack "label,sample,entity,numSamples,value")
                  . CL.dropWhile (/= '\n')
                  . CSV.encodeDefaultOrderedByName
                  . concatMap V.toList
                  . Map.elems
                  $ result

    CL.putStrLn formatted

    return ()
