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
newtype Field  = Field { unField :: T.Text } deriving (Show, Generic)
newtype Label  = Label { unLabel :: T.Text } deriving (Eq, Ord)
newtype Sample = Sample { unSample :: T.Text } deriving (Eq, Ord, Show)

-- Advanced

-- Algebraic
data Method = StandardScore deriving (Eq, Read, Show)

data Entity = Entity
                { _label     :: !T.Text
                , _sample    :: !T.Text
                , _entity    :: !T.Text
                , _value     :: !Double
                }
              deriving (Show, Generic)
makeLenses ''Entity

instance FromNamedRecord Entity
instance ToNamedRecord Entity
instance DefaultOrdered Entity
