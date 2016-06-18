{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeOperators #-}

module Server
  (
    app
  ) where

import Types
import qualified Database as DB

import Control.Monad.IO.Class
import Network.Wai
import Servant
import Database.PostgreSQL.Simple

type ProjectsApi = "projects"
                :> Get '[JSON] [Project]

type ProjectHoursApi = "hours"
                    :> Capture "id" Int
                    :> QueryParam "month" Int
                    :> QueryParam "year" Int
                    :> Get '[JSON, ExcelCSV] (Headers '[Header "Content-Disposition" String] ProjectHours)

type Api = ProjectsApi
      :<|> ProjectHoursApi

projectHours :: Connection -> Server ProjectHoursApi
projectHours conn pid (Just mon) (Just year) = do
  hours' <- liftIO (DB.projectHours conn pid mon year)
  let contentDispHeader = "attachment; filename=hours-" ++ show year ++ "-" ++ show mon ++ ".csv"
  (return . addHeader contentDispHeader) hours'
projectHours _ _ _ _ = throwError err400 { errBody = "Missing `month` or `year` parameter" }

projects :: Connection -> Server ProjectsApi
projects conn = liftIO (DB.projects conn)

myApi :: Proxy Api
myApi = Proxy

app :: Connection -> Application
app conn = serve myApi (projects conn :<|> projectHours conn)
