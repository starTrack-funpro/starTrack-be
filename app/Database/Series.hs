{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Series where

import Data.Aeson (ToJSON)
import qualified Data.Aeson.Types as A
import Database.Chapter
import Database.Db
import Database.Episode
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Happstack.Server

data SeriesType = TVSeries | Film | Comic | Novel
  deriving (Generic, Enum, Show, Read, ToJSON, Eq)

instance FromField SeriesType where
  fromField f mData =
    case mData of
      Just "TVSeries" -> return TVSeries
      Just "Film" -> return Film
      Just "Comic" -> return Comic
      Just "Novel" -> return Novel
      _ -> returnError ConversionFailed f "Invalid SeriesType value"

instance ToField SeriesType where
  toField :: SeriesType -> Action
  toField TVSeries = toField ("TVSeries" :: String)
  toField Film = toField ("Film" :: String)
  toField Comic = toField ("Comic" :: String)
  toField Novel = toField ("Novel" :: String)

data Series = Series
  { id :: Int,
    title :: String,
    year :: Int,
    rating :: Double,
    description :: String,
    seriesType :: SeriesType,
    imageUrl :: String
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

data TrackingInfo = EpisodeTracking EpisodeWithUserEpisode | ChapterTracking ChapterWithUserChapter
  deriving (Generic)

instance ToJSON TrackingInfo where
  toJSON (EpisodeTracking episode) = A.toJSON episode
  toJSON (ChapterTracking chapter) = A.toJSON chapter

data SeriesTracking = SeriesTracking
  { series :: Series,
    trackingInfo :: [TrackingInfo]
  }
  deriving (Generic, ToJSON)

getAllSeries conn title (Just seriesType) = query conn q ("%" ++ title ++ "%", seriesType) :: IO [Series]
  where
    q = "SELECT * FROM \"Series\" WHERE UPPER(title) LIKE UPPER(?) AND type=?"
getAllSeries conn title Nothing = query conn q ["%" ++ title ++ "%"] :: IO [Series]
  where
    q = "SELECT * FROM \"Series\" WHERE UPPER(title) LIKE UPPER(?)"

getSeriesById conn id = queryOne conn q [id] :: IO (Maybe Series)
  where
    q = "SELECT * FROM \"Series\" WHERE id = ?"

-- do
-- fetched <- query conn "SELECT * FROM \"Series\" WHERE id = ?" [id] :: IO [Series]

-- if null fetched
--   then return Nothing
--   else return $ Just $ head fetched

addNewSeries conn (Series _ title year rating description seriesType imageUrl) = execute conn q (title, year, rating, description, seriesType, imageUrl)
  where
    q = "INSERT INTO \"Series\" (title, year, rating, description, type, \"imageUrl\") VALUES (?, ?, ?, ?, ?, ?)"

updateSeries conn (Series id title year rating description seriesType imageUrl) = execute conn q (title, year, rating, description, seriesType, imageUrl, id)
  where
    q = "UPDATE \"Series\" SET title=?, year=?, rating=?, description=?, type=?, \"imageUrl\"=? WHERE id=?"