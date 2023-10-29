{-# LANGUAGE OverloadedStrings #-}

module Main where

import Auth
import Configuration.Dotenv
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import Db
import GHC.Generics
import Happstack.Server
import Response

main :: IO ()
main = do
  putStrLn "===== Server Started! ====="

  loadFile defaultConfig

  databaseURL <- readDbUrl
  conn <- connectPostgreSQL databaseURL

  simpleHTTP nullConf $ routes conn

routes :: Connection -> ServerPartT IO Response
routes conn =
  msum
    [ dir "hello" $ ok $ msgResponse "Hello",
      dir "auth" $ authRoutes conn
    ]