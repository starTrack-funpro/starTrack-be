{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Series where

import Data.Aeson (ToJSON)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Happstack.Server

data SeriesType = TVSeries | Film | Comic | Novel
  deriving (Generic, Enum, Show, Read, ToJSON)

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

getAllSeries conn = query_ conn "SELECT id, title, year, rating, description, type FROM \"Series\"" :: IO [Series]

getSeriesById conn id = do
  fetched <- query conn "SELECT id, title, year, rating, description, type FROM \"Series\" WHERE id = ?" [id] :: IO [Series]

  if null fetched
    then return Nothing
    else return $ Just $ head fetched

addNewSeries conn (Series _ title year rating description seriesType imageUrl) =
  execute conn "INSERT INTO \"Series\" (title, year, rating, description, type, \"imageUrl\") VALUES (?, ?, ?, ?, ?, ?)" (title, year, rating, description, seriesType, imageUrl)