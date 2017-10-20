{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}

module Server
  (
    app
  ) where

import Types
import qualified Database as DB

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day)
import Database.PostgreSQL.Simple hiding ((:.))
import Network.Wai
import Network.Wai.Middleware.Cors
import Servant
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import Jose.Jws
import Jose.Jwt

validateJwt :: Text -> ByteString -> Handler Jws
validateJwt jwtSecret authHeader =
  let jwt = T.drop (T.length "Bearer ") (cs authHeader)
  in case hmacDecode (cs jwtSecret) (cs jwt) of
        Right verifiedJwt -> return verifiedJwt
        Left err          -> throwError (err403 { errBody = "Invalid signature: " <> (cs . show) err })

authHandler :: Text -> AuthHandler Request Jws
authHandler jwtSecret =
  let handler req = case lookup "authorization" (requestHeaders req) of
        Nothing -> throwError (err401 { errBody = "Missing authorization header" })
        Just authCookieKey -> validateJwt jwtSecret authCookieKey
  in mkAuthHandler handler

type ProjectsApi = "projects"
                :> Get '[JSON] [Project]

type ProjectHoursApi = "hours"
                    :> Capture "id" Text
                    :> QueryParam "start_date" Day
                    :> QueryParam "end_date" Day
                    :> Get '[ExcelCSV, JSON] ProjectHours

type TimeTrackingStatusApi = "time_tracking_status"
                          :> QueryParam "start_date" Text
                          :> QueryParam "end_date" Text
                          :> Get '[ExcelCSV, JSON] TimeTrackingStatus

type VisibilityApi = "visibility"
                  :> QueryParam "start_date" Text
                  :> QueryParam "end_date" Text
                  :> Get '[ExcelCSV, JSON] [Visibility]

type HealthApi = "health"
              :> Get '[JSON] Bool

type Api = AuthProtect "jwt-auth" :> ProjectsApi
      :<|> AuthProtect "jwt-auth" :> ProjectHoursApi
      :<|> AuthProtect "jwt-auth" :> TimeTrackingStatusApi
      :<|> AuthProtect "jwt-auth" :> VisibilityApi
      :<|> HealthApi

type instance AuthServerData (AuthProtect "jwt-auth") = Jws

genAuthServerContext :: Text -> Context (AuthHandler Request Jws ': '[])
genAuthServerContext jwtSecret = authHandler jwtSecret :. EmptyContext

server :: Connection -> Server Api
server conn = const (projects conn)
                :<|> const (projectHours conn)
                :<|> const (timeTrackingStatus conn)
                :<|> const (visibility conn)
                :<|> health conn

projectHours :: Connection -> Server ProjectHoursApi
projectHours conn pid (Just start_date) (Just end_date) =
  liftIO (DB.projectHours conn pid start_date end_date) >>= \case
    Just hours' -> return hours'
    Nothing -> throwError err404 { errBody = "Project not found" }
projectHours _ _ _ _ = throwError err400 { errBody = "Missing `start_date` and/or `end_date` parameter" }

projects :: Connection -> Server ProjectsApi
projects conn = liftIO (DB.projects conn)

timeTrackingStatus :: Connection -> Server TimeTrackingStatusApi
timeTrackingStatus conn (Just start) (Just end) =
  liftIO (DB.timeTrackingStatus conn (cs start) (cs end))
timeTrackingStatus _ _ _ = throwError err400 { errBody = "missing `start_date` or `end_date` parameter" }

visibility :: Connection -> Server VisibilityApi
visibility conn (Just start) (Just end) = 
  liftIO (DB.visibility conn (cs start) (cs end))
visibility _ _ _ = throwError err400 { errBody = "missing `start_date` or `end_date` parameter" }
health :: Connection -> Server HealthApi
health = liftIO . DB.health

myApi :: Proxy Api
myApi = Proxy

corsPolicy :: Request -> Maybe CorsResourcePolicy
corsPolicy _ = Just $ simpleCorsResourcePolicy
                        { corsMethods = ["GET", "HEAD"]
                        , corsRequestHeaders = "authorization":simpleHeaders
                        }

app :: Text -> Connection -> Application
app jwtSecret conn = cors corsPolicy $
  serveWithContext myApi
    (genAuthServerContext jwtSecret)
    (server conn)
