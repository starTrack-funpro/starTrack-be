module SeriesChapter where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Database.Chapter
import Database.PostgreSQL.Simple
import Database.Series
import Happstack.Server
import JWT
import Utils

seriesChapterRoutes conn seriesId =
  msum
    [ nullDir >> getAllChapterBySeriesIdHandler conn seriesId,
      nullDir >> addChapterHandler conn seriesId
    ]

getAllChapterBySeriesIdHandler :: Connection -> Int -> ServerPart Response
getAllChapterBySeriesIdHandler conn seriesId = do
  method GET

  fetchedChapter <- liftIO $ getAllChapterBySeriesId conn seriesId

  ok $ defaultResponse $ encode fetchedChapter

addChapterHandler :: Connection -> Int -> ServerPart Response
addChapterHandler conn seriesId = authenticate $ decodeRequestBody $ do
  method POST

  formTitle <- look "title"
  formChapterNo <- look "chapterNo"
  formPageFrom <- look "pageFrom"
  formPageTo <- look "pageTo"

  fetchedSeries <- liftIO $ getSeriesById conn seriesId

  case fetchedSeries of
    Just fetched -> do
      let newChapter = Chapter 0 formTitle (read formChapterNo) (read formPageFrom) (read formPageTo) seriesId

      liftIO $ addNewChapter conn newChapter

      ok $ msgResponse "Successfully add new chapter"
    Nothing -> notFound $ msgResponse "Series not found"