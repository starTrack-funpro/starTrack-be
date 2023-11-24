{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

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
