{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Chapter where

import Data.Aeson
import Database.Db
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.UserChapter
import GHC.Generics

data Chapter = Chapter
  { title :: String,
    no :: Int,
    pageFrom :: Int,
    pageTo :: Int,
    seriesId :: Int
  }
  deriving (Generic, Show, ToRow, FromRow, ToJSON)

data ChapterWithUserChapter = ChapterWithUserChapter
  { chapter :: Chapter,
    userChapter :: Maybe UserChapter
  }
  deriving (Generic, ToJSON)

instance FromRow ChapterWithUserChapter where
  fromRow = ChapterWithUserChapter <$> fromRow <*> parseUserChapter
    where
      parseUserChapter = do
        maybeUser <- field
        maybeSeriesId <- field
        maybeChapterNo <- field
        maybeLastReadPage <- field
        case (maybeUser, maybeSeriesId, maybeChapterNo, maybeLastReadPage) of
          (Just user, Just seriesId, Just chapterNo, Just lastReadPage) -> return $ Just (UserChapter user seriesId chapterNo lastReadPage)
          _ -> return Nothing

getAllChapterBySeriesId conn seriesId = query conn q [seriesId] :: IO [Chapter]
  where
    q = "SELECT title, no, \"pageFrom\", \"pageTo\", \"seriesId\" FROM \"Chapter\" WHERE \"seriesId\" = ? ORDER BY no ASC"

getChapterByNo conn seriesId chapterNo = queryOne conn q (seriesId, chapterNo) :: IO (Maybe Chapter)
  where
    q = "SELECT title, no, \"pageFrom\", \"pageTo\", \"seriesId\" FROM \"Chapter\" WHERE \"seriesId\" = ? AND no = ?"

getAllTrackedChapterBySeriesId conn user seriesId = query conn q (user, seriesId) :: IO [ChapterWithUserChapter]
  where
    q =
      "SELECT c.title, c.no, c.\"pageFrom\", c.\"pageTo\", c.\"seriesId\", \
      \uc.user, uc.\"seriesId\", uc.\"chapterNo\", uc.\"lastReadPage\" \
      \FROM \"Chapter\" c LEFT JOIN \"UserChapter\" uc \
      \ON c.no = uc.\"chapterNo\" AND c.\"seriesId\" = uc.\"seriesId\" AND uc.user = ? \
      \WHERE c.\"seriesId\" = ? \
      \ORDER BY c.no ASC"

addNewChapter conn (Chapter title no pageFrom pageTo seriesId) = execute conn q (title, no, pageFrom, pageTo, seriesId)
  where
    q = "INSERT INTO \"Chapter\" (title, no, \"pageFrom\", \"pageTo\", \"seriesId\") VALUES (?, ?, ?, ?, ?)"
