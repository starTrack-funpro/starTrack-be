module Series where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.Series
import Happstack.Server
import Utils

seriesRoutes conn =
  msum
    [ nullDir >> allSeries conn,
      nullDir >> addSeries conn
    ]

allSeries :: Connection -> ServerPart Response
allSeries conn = do
  method GET

  series <- liftIO $ getAllSeries conn

  ok $ defaultResponse $ encode series

addSeries :: Connection -> ServerPart Response
addSeries conn = decodeRequestBody $ do
  method POST

  formTitle <- look "title"
  formYear <- look "year"
  formRating <- look "rating"
  formDesc <- look "description"
  formType <- look "type"

  let newSeries = Series 0 formTitle (read formYear) (read formRating) formDesc (read formType)

  liftIO $ addNewSeries conn newSeries

  ok $ msgResponse "Successfully add new series"