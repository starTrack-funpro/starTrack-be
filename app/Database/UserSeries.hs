{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.UserSeries where

import Data.Aeson
import Database.PostgreSQL.Simple
import qualified Database.Series as S
import GHC.Generics

data UserSeries = UserSeries
  { id :: Int,
    username :: String,
    seriesId :: Int
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

addNewUserSeries conn (UserSeries _ username seriesId) =
  execute conn "INSERT INTO \"UserSeries\" (username, \"seriesId\") VALUES (?, ?)" (username, seriesId)

getUserSeries conn username seriesId = do
  fetched <- query conn "SELECT * FROM \"UserSeries\" WHERE username = ? AND \"seriesId\" = ?" (username, seriesId) :: IO [UserSeries]

  if null fetched
    then return Nothing
    else return $ Just $ head fetched

getAllUserSeries conn username =
  query
    conn
    "SELECT * FROM \"Series\" WHERE id IN (SELECT \"seriesId\" FROM \"UserSeries\" WHERE username = ?)"
    [username] ::
    IO [S.Series]
