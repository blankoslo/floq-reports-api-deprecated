{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types
  (
    Project(..),
    EmployeeHours(..),
    ProjectHours(..),
    EmployeeLoggedHours(..),
    TimeTrackingStatus(..),
    ExcelCSV
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Char (ord)
import Data.Monoid ((<>))
import GHC.Generics
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V

import Codec.Text.IConv (convert)
import Data.Aeson.Types (ToJSON (..))
import Data.String.Conversions (cs)
import Data.Csv
import Data.Time.Calendar (showGregorian)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Time (Date, Unbounded (..))
import Servant.API.ContentTypes
import Network.HTTP.Media ((//), (/:))

-- A project
data Project = Project {
    projectId :: Text
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
  , employeeHours :: Vector Double
  } deriving (Generic, Show)

instance ToJSON EmployeeHours
instance FromRow EmployeeHours

-- An employee row is the employee name prepended to the list of hours
instance ToRecord EmployeeHours where
    toRecord (EmployeeHours name' hours') = toField name' `V.cons` toRecord hours''
      where hours'' :: Vector EuDecimal
            hours'' = V.map (EuDecimal) hours'

-- A project together with hours worked by employees
data ProjectHours = ProjectHours {
    project :: Project
  , hours :: [EmployeeHours]
  } deriving (Generic, Show)

instance ToJSON ProjectHours

data EmployeeLoggedHours = EmployeeLoggedHours {
    employee :: Text
  , availableHours :: Double
  , billableHours :: Double
  , nonBillableHours :: Double
  , unavailableHours :: Double
  , unregisteredDays :: Int
  , lastTimeEntryDate :: Maybe Date
  , lastTimeEntryCreated :: Maybe Date
  } deriving (Generic, Show)

instance ToJSON EmployeeLoggedHours
instance FromRow EmployeeLoggedHours

instance ToField Date where
  toField (Finite day) = (cs . showGregorian) day
  toField _            = error "Cannot represent infinity"

instance ToJSON Date where
  toJSON (Finite day) = toJSON day
  toJSON _            = error "Cannot represent infinity"

instance ToRecord EmployeeLoggedHours where
    toRecord (EmployeeLoggedHours name available billed nonBilled unavailable unregistered lastDate lastCreated) =
      let available'   = EuDecimal available
          billed'      = EuDecimal billed
          nonBilled'   = EuDecimal nonBilled
          unavailable' = EuDecimal unavailable
       in record [toField name, toField available', toField billed', toField nonBilled', toField unavailable', toField unregistered, toField lastDate, toField lastCreated]

newtype TimeTrackingStatus = TimeTrackingStatus [EmployeeLoggedHours]
  deriving (Generic, Show)

instance ToJSON TimeTrackingStatus

data ExcelCSV

utf16LEByteOrderMark :: ByteString
utf16LEByteOrderMark = "\xff\xfe"

instance Accept ExcelCSV where
  contentType _ = "text" // "csv" /: ("charset", "utf-16")

instance ToRecord a => MimeRender ExcelCSV [a] where
  mimeRender _ = encodeWith defaultEncodeOptions {
                        encDelimiter = fromIntegral (ord '\t')
                      , encUseCrLf = True
                      }

-- An Excel-friendly csv file starts with a UTF-16LE byte-order mark to indicate
-- that it's unicode, uses tabs as separators and CRLF line endings.
instance MimeRender ExcelCSV ProjectHours where
  mimeRender proxy ph = let newLine = "\r\n"
                            body = mimeRender proxy (project ph)
                                <> newLine
                                <> mimeRender proxy (hours ph)
                            encoded = convert "UTF-8" "UTF-16LE" body
                         in utf16LEByteOrderMark <> encoded

instance MimeRender ExcelCSV TimeTrackingStatus where
  mimeRender proxy (TimeTrackingStatus hours') =
    let body = mimeRender proxy hours'
     in utf16LEByteOrderMark <> convert "UTF-8" "UTF-16LE" body

-- A data type representing decimals that should use comma as a separator
newtype EuDecimal = EuDecimal Double

instance ToField EuDecimal where
  toField (EuDecimal d) = (cs . map replaceDot . show) d
    where replaceDot '.' = ','
          replaceDot c   = c
