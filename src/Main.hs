module Main where

import Server (app)

import Control.Exception (bracket)
import Data.String.Conversions (cs)
import Database.PostgreSQL.Simple (connectPostgreSQL, close)
import Network.Wai.Handler.Warp (run)
import Options.Applicative

data Config = Config { port :: Int
                     , dbUrl :: String }

config :: Parser Config
config = Config
  <$> option auto (value 3000 <> long "port" <> metavar "PORT" <> help "Listening port")
  <*> argument str (metavar "DB_URL" <> help "DB connection string (postgres://...)")

main :: IO ()
main = do
  let opts = info (helper <*> config) mempty

  config' <- execParser opts
  let connectionString = (cs . dbUrl) config'
  let port' = port config'

  bracket
    (connectPostgreSQL connectionString) -- acquire
    close                                -- release
    (run port' . app)                    -- use
