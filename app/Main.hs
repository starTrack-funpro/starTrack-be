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
import qualified Happstack.Server.Internal.Monads
import Response

main :: IO ()
main = do
  putStrLn "===== Server Started! ====="

  loadFile defaultConfig

  databaseURL <- readDbUrl
  conn <- connectPostgreSQL databaseURL

  simpleHTTP nullConf $ routes conn

routes :: Connection -> Happstack.Server.Internal.Monads.ServerPartT IO Response
routes conn =
  msum
    [ dir "hello" $ ok $ toResponse ("Hello" :: String),
      dir "auth" $ authRoutes conn
    ]

getAllUser conn = query_ conn "SELECT username, password, name FROM \"User\"" :: IO [User]