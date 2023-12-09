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

addNewUserEpisode conn (UserEpisode user seriesId episodeNo lastWatchTime) = execute conn q (user, seriesId, episodeNo, lastWatchTime)
  where
    q = "INSERT INTO \"UserEpisode\" (\"user\", \"seriesId\", \"episodeNo\", \"lastWatchTime\") VALUES (?, ?, ?, ?)"

updateLastWatchTime conn user seriesId episodeNo lastWatchTime = execute conn q (lastWatchTime, user, seriesId, episodeNo)
  where
    q = "UPDATE \"UserEpisode\" SET \"lastWatchTime\" = ? WHERE \"user\" = ? AND \"seriesId\" = ? AND \"episodeNo\" = ?"