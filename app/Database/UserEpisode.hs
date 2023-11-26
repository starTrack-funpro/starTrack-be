{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.UserEpisode where

import Data.Aeson
import Data.Time (TimeOfDay)
import Database.Db
import Database.PostgreSQL.Simple
import GHC.Generics

data UserEpisode = UserEpisode
  { user :: String,
    seriesId :: Int,
    episodeNo :: Int,
    lastWatchTime :: TimeOfDay
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

getUserEpisode conn user seriesId episodeNo = queryOne conn q (user, seriesId, episodeNo) :: IO (Maybe UserEpisode)
  where
    q = "SELECT \"user\", \"seriesId\", \"episodeNo\", \"lastWatchTime\" FROM \"UserEpisode\" WHERE \"user\" = ? AND \"seriesId\" = ? AND \"episodeNo\" = ?"

addNewUserEpisode conn (UserEpisode user seriesId episodeNo lastWatchTime) =
  execute conn "INSERT INTO \"UserEpisode\" (\"user\", \"seriesId\", \"episodeNo\", \"lastWatchTime\") VALUES (?, ?, ?, ?)" (user, seriesId, episodeNo, lastWatchTime)