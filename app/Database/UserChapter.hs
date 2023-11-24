{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.UserChapter where

import Data.Aeson
import Database.PostgreSQL.Simple
import GHC.Generics

data UserChapter = UserChapter
  { user :: String,
    seriesId :: Int,
    chapterNo :: Int,
    lastReadPage :: Int
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)
