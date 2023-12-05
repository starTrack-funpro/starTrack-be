{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Episode where

import Data.Aeson
import Data.Time
import Database.Db
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.UserEpisode
import GHC.Generics

data Episode = Episode
  { title :: String,
    no :: Int,
    duration :: TimeOfDay,
    seriesId :: Int
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

data EpisodeWithUserEpisode = EpisodeWithUserEpisode
  { episode :: Episode,
    userEpisode :: Maybe UserEpisode
  }
  deriving (Generic, ToJSON)

instance FromRow EpisodeWithUserEpisode where
  fromRow = EpisodeWithUserEpisode <$> fromRow <*> parseUserEpisode
    where
      parseUserEpisode = do
        maybeUser <- field
        maybeSeriesId <- field
        maybeEpisodeNo <- field
        maybeLastWatchTime <- field
        case (maybeUser, maybeSeriesId, maybeEpisodeNo, maybeLastWatchTime) of
          (Just user, Just seriesId, Just episodeNo, Just lastWatchTime) -> return $ Just (UserEpisode user seriesId episodeNo lastWatchTime)
          _ -> return Nothing

getAllEpisodeBySeriesId conn seriesId =
  query conn "SELECT title, no, duration, \"seriesId\" FROM \"Episode\" WHERE \"seriesId\" = ?" [seriesId] :: IO [Episode]

getEpisodeByNo conn seriesId episodeNo = queryOne conn q (seriesId, episodeNo) :: IO (Maybe Episode)
  where
    q = "SELECT title, no, duration, \"seriesId\" FROM \"Episode\" WHERE \"seriesId\" = ? AND no = ?"

getAllTrackedEpisodeBySeriesId conn user seriesId =
  query conn q (user, seriesId) :: IO [EpisodeWithUserEpisode]
  where
    q =
      "SELECT e.title, e.no, e.duration, e.\"seriesId\", ue.user, ue.\"seriesId\", ue.\"episodeNo\", ue.\"lastWatchTime\" \
      \FROM \"Episode\" e LEFT JOIN \"UserEpisode\" ue \
      \ON e.no = ue.\"episodeNo\" AND e.\"seriesId\" = ue.\"seriesId\" AND ue.user = ? \
      \WHERE e.\"seriesId\" = ?"

addNewEpisode conn (Episode title no duration seriesId) =
  execute conn "INSERT INTO \"Episode\" (title, no, duration, \"seriesId\") VALUES (?, ?, ?, ?)" (title, no, duration, seriesId)