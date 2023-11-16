module SeriesTrack where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.Series
import Database.User
import Database.UserSeries
import Happstack.Server
import JWT
import Utils

seriesTrackRoutes conn =
  msum
    [ nullDir >> getTrackedSeriesHandler conn,
      path $ \seriesId -> trackSeriesHandler conn seriesId
    ]

getTrackedSeriesHandler :: Connection -> ServerPartT IO Response
getTrackedSeriesHandler conn = authenticate $ do
  method GET
  maybeUsername <- getUsernameFromJwt

  case maybeUsername of
    Just username -> do
      user <- liftIO $ getUserByUsername conn username
      fetchedSeries <- liftIO $ getAllUserSeries conn username

      case (user, fetchedSeries) of
        (Just _, series) -> ok $ defaultResponse $ encode series
        (Nothing, _) -> unauthorizedResponse
    Nothing -> unauthorizedResponse

trackSeriesHandler :: Connection -> Int -> ServerPartT IO Response
trackSeriesHandler conn seriesId = authenticate $ do
  method POST
  maybeUsername <- getUsernameFromJwt

  case maybeUsername of
    Just username -> do
      user <- liftIO $ getUserByUsername conn username
      fetchedSeries <- liftIO $ getSeriesById conn seriesId
      fetchedUserSeries <- liftIO $ getUserSeries conn username seriesId

      response <- case (user, fetchedSeries, fetchedUserSeries) of
        (Just _, Just _, Nothing) -> do
          let userSeries = UserSeries 0 username seriesId False
          liftIO $ addNewUserSeries conn userSeries
          return "Successfully track series"
        (Just _, Just _, Just _) -> return "Series already tracked"
        (Nothing, _, _) -> return "Unauthorized"
        (_, Nothing, _) -> return "Series not found"

      case response of
        "Successfully track series" -> ok $ msgResponse response
        "Unauthorized" -> unauthorizedResponse
        "Series not found" -> notFound $ msgResponse response
        "Series already tracked" -> badRequest $ msgResponse response
    Nothing -> unauthorizedResponse
