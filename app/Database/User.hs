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
    name :: String,
    role :: String
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

getUserByUsername conn name = queryOne conn q [name] :: IO (Maybe User)
  where
    q = "SELECT username, password, name, role FROM \"User\" WHERE username = ?"

addNewUser conn (User username password name _) = execute conn q (username, password, name)
  where
    q = "INSERT INTO \"User\" VALUES (?, ?, ?)"