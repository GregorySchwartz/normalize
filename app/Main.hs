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

-- | Command line arguments
data Options = Options { labelField  :: Maybe T.Text
                                    <?> "The column containing the label for the entry."
                       , sampleField :: T.Text
                                    <?> "The column containing the sample for the entry."
                       , entityField :: T.Text
                                    <?> "The column containing the id for the entity in the entry."
                       , valueField  :: T.Text
                                    <?> "The column field containing the value for the entry."
                       , method      :: Maybe String
                                    <?> "([StandardScore]) The method for standardization of the samples."
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

    let entities = V.map ( csvRowToEntity
                            (V.head csvContents)
                            (fmap Field . unHelpful $ labelField opts)
                            (Field . unHelpful $ sampleField opts)
                            (Field . unHelpful $ entityField opts)
                            (Field . unHelpful $ valueField opts)
                         )
                    . V.tail
                    $ csvContents
        sampleMap = toSampleMap entities
        result    = normalize
                        (maybe StandardScore read . unHelpful . method $ opts)
                        sampleMap
        formatted = CL.append (CL.pack "label,sample,entity,value")
                  . CL.dropWhile (/= '\n')
                  . CSV.encodeDefaultOrderedByName
                  . concatMap V.toList
                  . Map.elems
                  $ result

    CL.putStrLn formatted

    return ()
