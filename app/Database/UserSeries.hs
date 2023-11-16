{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.UserSeries where

import Data.Aeson
import Database.PostgreSQL.Simple
import GHC.Generics

data UserSeries = UserSeries
  { id :: Int,
    username :: String,
    seriesId :: Int,
    completed :: Bool
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

addNewUserSeries conn (UserSeries _ username seriesId _) =
  execute conn "INSERT INTO \"UserSeries\" (username, \"seriesId\") VALUES (?, ?)" (username, seriesId)

getUserSeries conn username seriesId = do
  fetched <- query conn "SELECT * FROM \"UserSeries\" WHERE username = ? AND \"seriesId\" = ?" (username, seriesId) :: IO [UserSeries]

  if null fetched
    then return Nothing
    else return $ Just $ head fetched