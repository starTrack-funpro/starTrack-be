{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.User where

import Data.Aeson
import Database.Db
import Database.PostgreSQL.Simple
import GHC.Generics
import Happstack.Server

data User = User
  { username :: String,
    password :: String,
    name :: String
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

getUserByUsername conn name = queryOne conn q [name] :: IO (Maybe User)
  where
    q = "SELECT username, password, name FROM \"User\" WHERE username = ?"

addNewUser conn (User username password name) = execute conn "INSERT INTO \"User\" VALUES (?, ?, ?)" (username, password, name)