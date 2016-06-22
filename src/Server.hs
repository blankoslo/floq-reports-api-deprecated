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
import Data.Monoid ((<>))
import Data.String.Conversions (cs)
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple hiding ((:.))
import Network.Wai
import Network.Wai.Middleware.Cors
import Servant
import Servant.Server.Experimental.Auth (AuthServerData, AuthHandler, mkAuthHandler)
import Web.JWT (JWT, VerifiedJWT)
import qualified Web.JWT as Jwt

validateJwt :: Text -> ByteString -> Handler (JWT VerifiedJWT)
validateJwt jwtSecret authHeader =
  let jwt = T.drop (T.length "Bearer ") (cs authHeader)
   in case Jwt.decodeAndVerifySignature (Jwt.secret jwtSecret) jwt of
        Just verifiedJwt -> return verifiedJwt
        Nothing -> throwError (err403 { errBody = "Invalid signature" <> cs jwt })

authHandler :: Text -> AuthHandler Request (JWT VerifiedJWT)
authHandler jwtSecret =
  let handler req = case lookup "authorization" (requestHeaders req) of
        Nothing -> throwError (err401 { errBody = "Missing authorization header" })
        Just authCookieKey -> validateJwt jwtSecret authCookieKey
  in mkAuthHandler handler

type ProjectsApi = "projects"
                :> Get '[JSON] [Project]

type ProjectHoursApi = "hours"
                    :> Capture "id" Int
                    :> QueryParam "year" Int
                    :> QueryParam "month" Int
                    :> Get '[ExcelCSV, JSON] (Headers '[Header "Content-Disposition" String] ProjectHours)

type Api = AuthProtect "jwt-auth" :> (ProjectsApi :<|> ProjectHoursApi)

type instance AuthServerData (AuthProtect "jwt-auth") = JWT VerifiedJWT

genAuthServerContext :: Text -> Context (AuthHandler Request (JWT VerifiedJWT) ': '[])
genAuthServerContext jwtSecret = authHandler jwtSecret :. EmptyContext

genAuthServer :: Connection -> Server Api
genAuthServer conn _ = projects conn :<|> projectHours conn

projectHours :: Connection -> Server ProjectHoursApi
projectHours conn pid (Just year) (Just mon) = do
  hours' <- liftIO (DB.projectHours conn pid mon year)
  let contentDispHeader = "attachment; filename=hours-" ++ show year ++ "-" ++ show mon ++ ".csv"
  (return . addHeader contentDispHeader) hours'
projectHours _ _ _ _ = throwError err400 { errBody = "Missing `month` or `year` parameter" }

projects :: Connection -> Server ProjectsApi
projects conn = liftIO (DB.projects conn)

myApi :: Proxy Api
myApi = Proxy

app :: Text -> Connection -> Application
app jwtSecret conn = simpleCors $
  serveWithContext myApi
    (genAuthServerContext jwtSecret)
    (genAuthServer conn)
