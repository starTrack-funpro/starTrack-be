module Series where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.Series
import qualified Database.User as U
import Happstack.Server
import JWT
import SeriesChapter (seriesChapterRoutes)
import Text.Read (readMaybe)
import Utils

seriesRoutes conn =
  msum
    [ nullDir >> getAllSeriesHandler conn,
      nullDir >> addSeriesHandler conn,
      path $ \id -> dir "chapter" $ seriesChapterRoutes conn id,
      path $ \id -> getSeriesByIdHandler conn id,
      path $ \id -> updateSeriesHandler conn id
    ]

getAllSeriesHandler :: Connection -> ServerPart Response
getAllSeriesHandler conn = do
  method GET

  queryTitle <- look "title"
  queryTypeString <- look "type"

  let queryType = readMaybe queryTypeString :: Maybe SeriesType

  fetchedSeries <- liftIO $ getAllSeries conn queryTitle queryType

  ok $ defaultResponse $ encode fetchedSeries

getSeriesByIdHandler :: Connection -> Int -> ServerPart Response
getSeriesByIdHandler conn id = do
  method GET

  fetchedSeries <- liftIO $ getSeriesById conn id

  case fetchedSeries of
    Just fetched -> ok $ defaultResponse $ encode fetched
    Nothing -> notFound $ msgResponse "Series not found"

addSeriesHandler :: Connection -> ServerPart Response
addSeriesHandler conn = authenticate $ decodeRequestBody $ do
  method POST

  formTitle <- look "title"
  formYear <- look "year"
  formRating <- look "rating"
  formDesc <- look "description"
  formType <- look "type"
  formImageUrl <- look "imageUrl"

  let newSeries = Series 0 formTitle (read formYear) (read formRating) formDesc (read formType) formImageUrl

  liftIO $ addNewSeries conn newSeries

  ok $ msgResponse "Successfully add new series"

updateSeriesHandler :: Connection -> Int -> ServerPart Response
updateSeriesHandler conn seriesId = authenticate $ decodeRequestBody $ do
  method PATCH

  formTitle <- look "title"
  formYear <- look "year"
  formRating <- look "rating"
  formDesc <- look "description"
  formType <- look "type"
  formImageUrl <- look "imageUrl"

  let updatedSeries = Series seriesId formTitle (read formYear) (read formRating) formDesc (read formType) formImageUrl

  liftIO $ updateSeries conn updatedSeries

  ok $ msgResponse "Successfully update series"
