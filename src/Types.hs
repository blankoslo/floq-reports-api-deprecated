{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language DuplicateRecordFields #-}

module Types
  (
    Project(..),
    EmployeeHours(..),
    ProjectHours(..),
    ExcelCSV
  ) where

import Data.Char (ord)
import Data.Monoid ((<>))
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

-- A project
data Project = Project {
    id :: Int
  , name :: Text
  , customerName :: Text
  } deriving (Generic, Show)

instance ToRecord Project
instance FromRow Project
instance ToJSON Project

instance MimeRender ExcelCSV Project where
  mimeRender _ (Project _ name' _) = cs name'

-- A list of hours worked for an employee
data EmployeeHours = EmployeeHours {
    name :: Text
  , minutes :: Vector Int
  } deriving (Generic, Show)

instance ToJSON EmployeeHours
instance FromRow EmployeeHours

-- An employee row is the employee name prepended to the list of hours
instance ToRecord EmployeeHours where
    toRecord (EmployeeHours name' minutes') = toField name' `V.cons` toRecord hours'
      where hours' :: Vector EuDecimal
            hours' = V.map (EuDecimal . (/60.0) . fromIntegral) minutes'

-- A project together with hours worked by employees
data ProjectHours = ProjectHours {
    project :: Project
  , hours :: [EmployeeHours]
  } deriving (Generic, Show)

instance ToJSON ProjectHours

data ExcelCSV

instance Accept ExcelCSV where
  contentType _ = "text" // "csv" /: ("charset", "utf-8")

instance ToRecord a => MimeRender ExcelCSV [a] where
  mimeRender _ = encodeWith options
    where options = defaultEncodeOptions {
                        encDelimiter = fromIntegral (ord '\t')
                      , encUseCrLf = True
                      }

-- An Excel-friendly csv file starts with a byte-order mark to indicate that
-- it's utf-8, then `sep=<tab>` to specify that tab is used as the separator.
-- Line endings are CRLF.
instance MimeRender ExcelCSV ProjectHours where
  mimeRender proxy ph = utf8ByteOrderMark
                     <> separatorHeader
                     <> newLine
                     <> mimeRender proxy (project ph)
                     <> newLine
                     <> mimeRender proxy (hours ph)
    where utf8ByteOrderMark = "\xef\xbb\xbf"
          separatorHeader = "sep=\t"
          newLine = "\r\n"


-- A data type representing decimals that should use comma as a separator
newtype EuDecimal = EuDecimal Double

instance ToField EuDecimal where
  toField (EuDecimal d) = (cs . map replaceDot . show) d
    where replaceDot '.' = ','
          replaceDot c   = c
