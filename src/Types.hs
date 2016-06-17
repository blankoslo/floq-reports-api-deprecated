{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types
  (
    Project(..),
    EmployeeHours(..),
    CSV
  ) where

import Data.Char (ord)
import GHC.Generics
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Aeson.Types (ToJSON)
import Data.String.Conversions (cs)
import Data.Csv
import Database.PostgreSQL.Simple.FromRow
import Servant.API.ContentTypes
import Network.HTTP.Media ((//), (/:))

data Project = Project {
    id :: Int
  , name :: Text
  , customerName :: Text
  } deriving (Generic, Show)

instance ToRecord Project
instance FromRow Project
instance ToJSON Project

data EmployeeHours = EmployeeHours {
    name :: Text
  , minutes :: Vector Int
  } deriving (Generic, Show)

instance ToJSON EmployeeHours
instance FromRow EmployeeHours

-- An employee row is the employee name prepended to the list of hours
instance ToRecord EmployeeHours where
    toRecord (EmployeeHours name' minutes') = toField name' `V.cons` toRecord hours
      where hours :: Vector EuDecimal
            hours = V.map (EuDecimal . (/60.0) . fromIntegral) minutes'

data CSV

instance Accept CSV where
  contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance ToRecord a => MimeRender CSV [a] where
  mimeRender _ = encodeWith options
    where options = defaultEncodeOptions { encDelimiter = fromIntegral (ord '\t') }

-- a data type representing decimals that should use comma as a separator
newtype EuDecimal = EuDecimal Double

instance ToField EuDecimal where
  toField (EuDecimal d) = (cs . map replaceDot . show) d
    where replaceDot '.' = ','
          replaceDot c   = c
