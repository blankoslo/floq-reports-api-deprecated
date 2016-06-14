{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server
  (
    app
  ) where

import Types
import qualified Database as DB

import Control.Monad.IO.Class
import Data.Aeson.Types
import Network.Wai
import Servant
import Servant.Server
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Text.InterpolatedString.Perl6 (q)

type ProjectsApi = "projects"
                :> Get '[JSON, CSV] [Project]

type ProjectHoursApi = "hours"
                    :> Capture "id" Int
                    :> QueryParam "month" Int
                    :> QueryParam "year" Int
                    :> Get '[JSON, CSV] (Headers '[Header "Content-Disposition" String] [EmployeeHours])

type Api = ProjectsApi
      :<|> ProjectHoursApi

type MyHandler = Get '[JSON] (Headers '[Header "X-An-Int" Int] Double)

myHandler :: Server MyHandler
myHandler = return $ addHeader 1797 3.14

hours :: Connection -> Server ProjectHoursApi
hours conn id (Just mon) (Just year) = do
  hours <- liftIO (DB.employeeHours conn id mon year)
  let contentDispHeader = "attachment; filename=hours-" ++ show year ++ "-" ++ show mon ++ ".csv"
  (return . addHeader contentDispHeader) hours
hours conn id _ _ = throwError err400 { errBody = "Missing `month` or `year` parameter" }

projects :: Connection -> Server ProjectsApi
projects conn = liftIO (DB.projects conn)

myApi :: Proxy Api
myApi = Proxy

app :: Connection -> Application
app conn = serve myApi (projects conn :<|> hours conn)
