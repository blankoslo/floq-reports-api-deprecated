{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}

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

import Codec.Text.IConv (convert)
import Data.Aeson.Types (ToJSON)
import Data.String.Conversions (cs)
import Data.Csv
import Database.PostgreSQL.Simple.FromRow
import Servant.API.ContentTypes
import Network.HTTP.Media ((//), (/:))

-- A project
data Project = Project {
    projectId :: Int
  , projectName :: Text
  , customerName :: Text
  } deriving (Generic, Show)

instance ToRecord Project
instance FromRow Project
instance ToJSON Project

instance MimeRender ExcelCSV Project where
  mimeRender _ (Project _ name' _) = cs name'

-- A list of hours worked for an employee
data EmployeeHours = EmployeeHours {
    employeeName :: Text
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
  contentType _ = "text" // "csv" /: ("charset", "utf-16")

instance ToRecord a => MimeRender ExcelCSV [a] where
  mimeRender _ = encodeWith options
    where options = defaultEncodeOptions {
                        encDelimiter = fromIntegral (ord '\t')
                      , encUseCrLf = True
                      }

-- An Excel-friendly csv file starts with a UTF-16LE byte-order mark to indicate
-- that it's unicode, uses tabs as separators and CRLF line endings.
instance MimeRender ExcelCSV ProjectHours where
  mimeRender proxy ph = let utf16LEByteOrderMark = "\xff\xfe"
                            newLine = "\r\n"
                            body = mimeRender proxy (project ph)
                                <> newLine
                                <> mimeRender proxy (hours ph)
                            encoded = convert "UTF-8" "UTF-16LE" body
                         in utf16LEByteOrderMark <> encoded

-- A data type representing decimals that should use comma as a separator
newtype EuDecimal = EuDecimal Double

instance ToField EuDecimal where
  toField (EuDecimal d) = (cs . map replaceDot . show) d
    where replaceDot '.' = ','
          replaceDot c   = c
