module Main where

import Server (app)

import Control.Exception (bracket)
import Data.String.Conversions (cs)
import Database.PostgreSQL.Simple (connectPostgreSQL, close)
import Network.Wai.Handler.Warp (run)
import Options.Applicative

data Config = Config { dbUrl :: String }

main :: IO ()
main = do
  connectionString <- (cs . dbUrl) <$> execParser opts
  bracket
    (connectPostgreSQL connectionString) -- acquire
    close                                -- release
    (run 8080 . app)                     -- use
  where
    parser = Config <$> argument str (metavar "DB_URL")
    opts = info parser mempty
