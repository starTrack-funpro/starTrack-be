{-# LANGUAGE OverloadedStrings #-}

module Main where

import Auth (authRoutes)
import Configuration.Dotenv
import Control.Monad
import Database.Db
import Database.PostgreSQL.Simple
import Happstack.Server
import Response
import Series (seriesRoutes)

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
      dir "auth" $ authRoutes conn,
      dir "series" $ seriesRoutes conn
    ]