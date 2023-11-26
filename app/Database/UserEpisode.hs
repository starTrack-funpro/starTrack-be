{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.UserEpisode where

import Data.Aeson
import Data.Time (TimeOfDay)
import Database.PostgreSQL.Simple
import GHC.Generics

data UserEpisode = UserEpisode
  { user :: String,
    seriesId :: Int,
    episodeNo :: Int,
    lastWatchTime :: TimeOfDay
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

getUserEpisode conn user seriesId episodeNo = do
  fetched <- query conn q (user, seriesId, episodeNo) :: IO [UserEpisode]

  if null fetched
    then return Nothing
    else return $ Just $ head fetched
  where
    q = "SELECT \"user\", \"seriesId\", \"episodeNo\", \"lastWatchTime\" FROM \"UserEpisode\" WHERE \"user\" = ? AND \"seriesId\" = ? AND \"episodeNo\" = ?"

addNewUserEpisode conn (UserEpisode user seriesId episodeNo lastWatchTime) =
  execute conn "INSERT INTO \"UserEpisode\" (\"user\", \"seriesId\", \"episodeNo\", \"lastWatchTime\") VALUES (?, ?, ?, ?)" (user, seriesId, episodeNo, lastWatchTime)