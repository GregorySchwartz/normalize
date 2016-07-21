{- normalize
Gregory W. Schwartz

Blah's the blah in the blah
-}
module Main where

-- Standard
import qualified Data.Map.Strict as Map

-- Cabal
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Csv as CSV
import Options.Applicative

-- Local
import qualified Types as Types
import Load
import Normalize

-- | Command line arguments
data Options = Options { labelField  :: Maybe Int
                       , sampleField :: Int
                       , entityField :: Int
                       , valueField  :: Int
                       , method      :: Types.Method
                       }

-- | Command line options
options :: Parser Options
options = Options
      <$> optional ( option auto
          ( long "label-field"
         <> short 'l'
         <> metavar "FIELD"
         <> help "The 1 indexed field containing the label for the entry."
          )
        )
      <*> option auto
          ( long "sample-field"
         <> short 's'
         <> metavar "FIELD"
         <> help "The 1 indexed field containing the sample for the entry."
          )
      <*> option auto
          ( long "entity-field"
         <> short 'e'
         <> metavar "FIELD"
         <> help "The 1 indexed field containing the id for the entity in the\
                 \ entry."
          )
      <*> option auto
          ( long "value-field"
         <> short 'v'
         <> metavar "FIELD"
         <> help "The 1 indexed field containing the value for the entry."
          )
      <*> option auto
          ( long "method"
         <> short 'm'
         <> metavar "[StandardScore]"
         <> value Types.StandardScore
         <> help "The method for standardization of the samples."
          )

mainFunc :: Options -> IO ()
mainFunc opts = do
    body <- fmap (either error id . CSV.decode CSV.HasHeader) CL.getContents

    let entities = V.map ( csvRowToEntity
                            (fmap Types.Field $ labelField opts)
                            (Types.Field $ sampleField opts)
                            (Types.Field $ entityField opts)
                            (Types.Field $ valueField opts)
                         )
                   body
        sampleMap = toSampleMap entities
        result    = normalize (method opts) sampleMap
        formatted = CL.append (CL.pack "label,sample,entity,value")
                  . CL.dropWhile (/= '\n')
                  . CSV.encodeDefaultOrderedByName
                  . concatMap V.toList
                  . Map.elems
                  $ result

    CL.putStrLn formatted

    return ()

main :: IO ()
main = execParser opts >>= mainFunc
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Normalizes the data (entities, for instance genes or\
                 \ proteins) by column (samples)."
     <> header "normalize, Gregory W. Schwartz" )
