{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
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
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.PostgreSQL.Simple hiding ((:.))
import Network.Wai
import Network.Wai.Middleware.Cors
import Servant
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import Jose.Jws
import Jose.Jwt

validateJwt :: Text -> ByteString -> Handler Jws
validateJwt jwtSecret authHeader =
  let jwt = cs authHeader
  in case hmacDecode (cs jwtSecret) jwt of
        Right verifiedJwt -> return verifiedJwt
        Left err          -> throwError (err403 { errBody = "Invalid signature: " <> (cs . show) err })

authHandler :: Text -> AuthHandler Request Jws
authHandler jwtSecret =
  let handler req = case lookup "authorization" (requestHeaders req) of
        Nothing -> case lookup "jwt" (queryString req) of
          Just (Just (jwt)) -> validateJwt jwtSecret jwt
          _                 -> throwError (err401 { errBody = "Missing authorization header" })
        Just jwt -> validateJwt jwtSecret $ BS.drop (BS.length "Bearer ") jwt
  in mkAuthHandler handler

type ProjectsApi = "projects"
                :> Get '[JSON] [Project]

type ProjectHoursApi = "hours"
                    :> Capture "id" Text
                    :> QueryParam "year" Int
                    :> QueryParam "month" Int
                    :> Get '[ExcelCSV, JSON] (Headers '[Header "Content-Disposition" String] ProjectHours)

type TimeTrackingStatusApi = "time_tracking_status"
                          :> QueryParam "start_date" Text
                          :> QueryParam "end_date" Text
                          :> Get '[ExcelCSV, JSON] (Headers '[Header "Content-Disposition" String] [TimeTrackingStatus])

type Api = AuthProtect "jwt-auth" :> ProjectsApi
      :<|> AuthProtect "jwt-auth" :> ProjectHoursApi
      :<|> AuthProtect "jwt-auth" :> TimeTrackingStatusApi

type instance AuthServerData (AuthProtect "jwt-auth") = Jws

genAuthServerContext :: Text -> Context (AuthHandler Request Jws ': '[])
genAuthServerContext jwtSecret = authHandler jwtSecret :. EmptyContext

genAuthServer :: Connection -> Server Api
genAuthServer conn = const (projects conn)
                :<|> const (projectHours conn)
                :<|> const (timeTrackingStatus conn)

projectHours :: Connection -> Server ProjectHoursApi
projectHours conn pid (Just year) (Just mon) = do
  hours' <- liftIO (DB.projectHours conn pid mon year)
  let mon' = if mon < 10 then '0':show mon else show mon
  let contentDispHeader = "attachment; filename=" <> cs pid <> "-" <> show year <> "-" <> mon' <> ".csv"
  (return . addHeader contentDispHeader) hours'
projectHours _ _ _ _ = throwError err400 { errBody = "Missing `month` or `year` parameter" }

projects :: Connection -> Server ProjectsApi
projects conn = liftIO (DB.projects conn)

timeTrackingStatus :: Connection -> Server TimeTrackingStatusApi
timeTrackingStatus conn (Just start) (Just end) = do
  let start' = cs start
  let end' = cs end
  status <- liftIO (DB.timeTrackingStatus conn start' end')
  let contentDispHeader = "attachment; filename=status" <> "-" <> cs start' <> "-" <> cs end' <> ".csv"
  (return . addHeader contentDispHeader) status
timeTrackingStatus _ _ _ = throwError err400 { errBody = "missing `start_date` or `end_date` parameter" }

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
    (genAuthServer conn)
