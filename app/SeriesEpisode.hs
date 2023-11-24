module SeriesEpisode where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Maybe
import Data.Time
import Database.Episode
import Database.PostgreSQL.Simple
import Database.Series
import Happstack.Server
import JWT
import Utils

seriesEpisodeRoutes conn seriesId =
  msum
    [ nullDir >> getAllEpisodeBySeriesIdHandler conn seriesId,
      nullDir >> addEpisodeHandler conn seriesId
    ]

getAllEpisodeBySeriesIdHandler :: Connection -> Int -> ServerPart Response
getAllEpisodeBySeriesIdHandler conn seriesId = do
  method GET

  fetchedChapter <- liftIO $ getAllEpisodeBySeriesId conn seriesId

  ok $ defaultResponse $ encode fetchedChapter

addEpisodeHandler :: Connection -> Int -> ServerPart Response
addEpisodeHandler conn seriesId = authenticate $ decodeRequestBody $ do
  method POST

  formTitle <- look "title"
  formNo <- look "no"
  formDuration <- look "duration"

  let parsedDuration = fromMaybe (TimeOfDay 0 0 0) (parseDuration formDuration)

  fetchedSeries <- liftIO $ getSeriesById conn seriesId

  case fetchedSeries of
    Just fetched -> do
      let newEpisode = Episode formTitle (read formNo) parsedDuration seriesId

      liftIO $ addNewEpisode conn newEpisode

      ok $ msgResponse "Successfully add new episode"
    Nothing -> notFound $ msgResponse "Series not found"

parseDuration :: String -> Maybe TimeOfDay
parseDuration = parseTimeM True defaultTimeLocale "%H:%M:%S"