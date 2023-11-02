{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Db where

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Text.Encoding
import Database.PostgreSQL.Simple
import GHC.Generics
import Happstack.Server.SimpleHTTP
import System.Environment

readDbUrl = do
  urlEnv <- getEnv "DATABASE_URL"
  return $ encodeUtf8 $ T.pack urlEnv

data User = User
  { username :: String,
    password :: String,
    name :: String
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

instance FromData User where
  fromData = do
    -- Define how to parse the request body into a User object
    username <- look "username"
    password <- look "password"
    name <- look "name"
    return (User username password name)

getUserByUsername conn name = do
  fetched <- query conn "SELECT username, password, name FROM \"User\" WHERE username = ?" [name] :: IO [User]

  if null fetched
    then return Nothing
    else return $ Just $ head fetched

addNewUser conn (User username password name) = execute conn "INSERT INTO \"User\" VALUES (?, ?, ?)" (username, password, name)