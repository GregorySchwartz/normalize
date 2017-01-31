{- Types
Gregory W. Schwartz

Collections the types used in the program
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

-- Standard
import GHC.Generics

-- Cabal
import qualified Data.Text as T
import Data.Csv
import Control.Lens

-- Local

-- Basic
newtype EntitySep        = EntitySep { unEntitySep :: T.Text }
newtype EntityName       = EntityName { unEntityName :: T.Text }
                           deriving (Eq, Ord)
newtype NumSamples       = NumSamples { unNumSamples :: Int }
newtype ValueThreshold   = ValueThreshold { unValueThreshold :: Double }
newtype StdDevThreshold  = StdDevThreshold { unStdDevThreshold :: Double }
newtype Base             = Base { unBase :: Double }
newtype Field            = Field { unField :: T.Text } deriving (Show, Generic)
newtype Label            = Label { unLabel :: T.Text } deriving (Eq, Ord)
newtype Sample           = Sample { unSample :: T.Text }
                           deriving (Eq, Ord, Show)
newtype Divisor          = Divisor { unDivisor :: Bool } deriving (Eq, Ord, Show)
newtype SynonymFlag      = SynonymFlag { unSynonymFlag :: Bool }
                           deriving (Eq, Ord, Show)
newtype NormSampleString = NormSampleString { unNormSampleString :: T.Text }
                           deriving (Eq, Ord, Show)

-- Advanced

-- Algebraic
data Method = StandardScore | UpperQuartile | None deriving (Eq, Read, Show)

data Entity = Entity
                { _label      :: !T.Text
                , _sample     :: !T.Text
                , _entity     :: !T.Text
                , _numSamples :: !Int
                , _value      :: !Double
                }
              deriving (Eq, Ord, Show, Generic)
makeLenses ''Entity

instance FromNamedRecord Entity
instance ToNamedRecord Entity
instance DefaultOrdered Entity
