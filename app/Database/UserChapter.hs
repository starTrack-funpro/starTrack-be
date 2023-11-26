{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.UserChapter where

import Data.Aeson
import Database.Db
import Database.PostgreSQL.Simple
import GHC.Generics

data UserChapter = UserChapter
  { user :: String,
    seriesId :: Int,
    chapterNo :: Int,
    lastReadPage :: Int
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

getUserChapter conn user seriesId chapterNo = queryOne conn q (user, seriesId, chapterNo) :: IO (Maybe UserChapter)
  where
    q = "SELECT \"user\", \"seriesId\", \"chapterNo\", \"lastReadPage\" FROM \"UserChapter\" WHERE \"user\" = ? AND \"seriesId\" = ? AND \"chapterNo\" = ?"

addNewUserChapter conn (UserChapter user seriesId chapterNo lastReadPage) =
  execute conn "INSERT INTO \"UserChapter\" (\"user\", \"seriesId\", \"chapterNo\", \"lastReadPage\") VALUES (?, ?, ?, ?)" (user, seriesId, chapterNo, lastReadPage)

updateLastReadPage conn user seriesId chapterNo lastReadPage =
  execute conn q (lastReadPage, user, seriesId, chapterNo)
  where
    q = "UPDATE \"UserChapter\" SET \"lastReadPage\" = ? WHERE \"user\" = ? AND \"seriesId\" = ? AND \"chapterNo\" = ?"