{-# LANGUAGE OverloadedStrings #-}

module Main where

import Auth (authRoutes)
import Configuration.Dotenv
import Control.Monad
import Database.Db
import Database.PostgreSQL.Simple
import Happstack.Server
import Series (seriesRoutes)
import SeriesTrack (seriesTrackRoutes)
import Utils

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
    [ preFlight,
      dir "hello" $ ok $ msgResponse "Hello",
      dir "auth" $ authRoutes conn,
      dir "series" $ dir "track" $ seriesTrackRoutes conn,
      dir "series" $ seriesRoutes conn
    ]

preFlight :: ServerPartT IO Response
preFlight = do
  method OPTIONS

  ok $ msgResponse "ok"