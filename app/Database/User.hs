{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.User where

import Data.Aeson
import Database.PostgreSQL.Simple
import GHC.Generics
import Happstack.Server

data User = User
  { username :: String,
    password :: String,
    name :: String
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

getUserByUsername conn name = do
  fetched <- query conn "SELECT username, password, name FROM \"User\" WHERE username = ?" [name] :: IO [User]

  if null fetched
    then return Nothing
    else return $ Just $ head fetched

addNewUser conn (User username password name) = execute conn "INSERT INTO \"User\" VALUES (?, ?, ?)" (username, password, name)