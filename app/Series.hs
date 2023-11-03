module Series where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.Series
import Happstack.Server
import JWT
import Utils

seriesRoutes conn =
  msum
    [ nullDir >> allSeries conn,
      nullDir >> addSeries conn,
      path $ \id -> series conn id
    ]

allSeries :: Connection -> ServerPart Response
allSeries conn = do
  method GET

  fetchedSeries <- liftIO $ getAllSeries conn

  ok $ defaultResponse $ encode fetchedSeries

series :: Connection -> Int -> ServerPart Response
series conn id = do
  method GET

  fetchedSeries <- liftIO $ getSeriesById conn id

  case fetchedSeries of
    Just fetched -> ok $ defaultResponse $ encode fetched
    Nothing -> notFound $ msgResponse "Series not found"

addSeries :: Connection -> ServerPart Response
addSeries conn = authenticate $ decodeRequestBody $ do
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