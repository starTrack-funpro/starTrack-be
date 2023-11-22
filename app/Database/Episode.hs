{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Episode where

import Data.Aeson
import Data.Time
import Database.PostgreSQL.Simple
import GHC.Generics

data Episode = Episode
  { id :: Int,
    title :: String,
    duration :: TimeOfDay,
    seriesId :: Int
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

getAllEpisodeBySeriesId conn seriesId =
  query conn "SELECT id, title, duration, \"seriesId\" FROM \"Episode\" WHERE \"seriesId\" = ?" [seriesId] :: IO [Episode]

addNewEpisode conn (Episode _ title duration seriesId) =
  execute conn "INSERT INTO \"Episode\" (title, duration, \"seriesId\") VALUES (?, ?, ?)" (title, duration, seriesId)