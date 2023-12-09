{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.UserSeries where

import Data.Aeson
import Database.Db
import Database.PostgreSQL.Simple
import qualified Database.Series as S
import GHC.Generics

data UserSeries = UserSeries
  { id :: Int,
    username :: String,
    seriesId :: Int
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

addNewUserSeries conn (UserSeries _ username seriesId) = execute conn q (username, seriesId)
  where
    q = "INSERT INTO \"UserSeries\" (username, \"seriesId\") VALUES (?, ?)"

getUserSeries conn username seriesId = queryOne conn q (username, seriesId) :: IO (Maybe UserSeries)
  where
    q = "SELECT * FROM \"UserSeries\" WHERE username = ? AND \"seriesId\" = ?"

getAllUserSeries conn username = query conn q [username] :: IO [S.Series]
  where
    q = "SELECT * FROM \"Series\" WHERE id IN (SELECT \"seriesId\" FROM \"UserSeries\" WHERE username = ?)"
