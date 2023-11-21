{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Chapter where

import Data.Aeson
import Database.PostgreSQL.Simple
import GHC.Generics

data Chapter = Chapter
  { id :: Int,
    title :: String,
    chapterNo :: Int,
    pageFrom :: Int,
    pageTo :: Int,
    seriesId :: Int
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

getAllChapterBySeriesId conn seriesId =
  query conn "SELECT id, title, \"chapterNo\", \"pageFrom\", \"pageTo\", \"seriesId\" FROM \"Chapter\" WHERE \"seriesId\" = ?" [seriesId] :: IO [Chapter]

addNewChapter conn (Chapter _ title chapterNo pageFrom pageTo seriesId) =
  execute conn "INSERT INTO \"Chapter\" (title, \"chapterNo\", \"pageFrom\", \"pageTo\", \"seriesId\") VALUES (?, ?, ?, ?, ?)" (title, chapterNo, pageFrom, pageTo, seriesId)
