module SeriesTrack where

import ChapterTrack
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Time
import Database.Chapter
import Database.Episode
import Database.PostgreSQL.Simple
import Database.Series
import qualified Database.Series as S
import Database.User
import Database.UserChapter
import Database.UserEpisode
import Database.UserSeries
import EpisodeTrack
import Happstack.Server
import JWT
import Utils

seriesTrackRoutes conn =
  msum
    [ nullDir >> getTrackedSeriesHandler conn,
      path $ \seriesId -> dir "episode" $ episodeTrackRoutes conn seriesId,
      path $ \seriesId -> dir "chapter" $ chapterTrackRoutes conn seriesId,
      path $ \seriesId -> getTrackedSeriesByIdHandler conn seriesId,
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

getTrackedSeriesByIdHandler :: Connection -> Int -> ServerPartT IO Response
getTrackedSeriesByIdHandler conn seriesId = authenticate $ do
  method GET

  maybeUsername <- getUsernameFromJwt

  case maybeUsername of
    Just username -> do
      user <- liftIO $ getUserByUsername conn username
      fetchedSeries <- liftIO $ getSeriesById conn seriesId
      fetchedUserSeries <- liftIO $ getUserSeries conn username seriesId

      case (user, fetchedSeries, fetchedUserSeries) of
        (Just _, Just series, Just _) -> do
          let sType = seriesType series
          -- for comparison
          -- if sType == TVSeries || sType == Film
          --   then do
          --     tracked <- liftIO $ getAllTrackedEpisodeBySeriesId conn username $ S.id series
          --     ok $ defaultResponse $ encode tracked
          --   else do
          --     tracked <- liftIO $ getAllTrackedChapterBySeriesId conn username $ S.id series
          --     ok $ defaultResponse $ encode tracked

          if sType == TVSeries || sType == Film
            then seriesTrackInfo EpisodeTracking getAllTrackedEpisodeBySeriesId
            else seriesTrackInfo ChapterTracking getAllTrackedChapterBySeriesId
          where
            seriesTrackInfo :: (a -> TrackingInfo) -> (Connection -> String -> Int -> IO [a]) -> ServerPartT IO Response
            seriesTrackInfo a f = do
              trackingInfo <- queryTracking a f
              ok $ defaultResponse $ encode $ SeriesTracking series trackingInfo
            queryTracking a b = do liftIO $ map a <$> queryFunction b
            queryFunction f = f conn username $ S.id series
        (Nothing, _, _) -> unauthorizedResponse
        (_, Nothing, _) -> notFound $ msgResponse "Series not found"
        (_, _, Nothing) -> badRequest $ msgResponse "Series not tracked"
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

      case (user, fetchedSeries, fetchedUserSeries) of
        (Just _, Just _, Nothing) -> do
          let userSeries = UserSeries 0 username seriesId
          liftIO $ addNewUserSeries conn userSeries
          ok $ msgResponse "Successfully track series"
        (Nothing, _, _) -> unauthorizedResponse
        (_, Nothing, _) -> notFound $ msgResponse "Series not found"
        (_, _, Just _) -> badRequest $ msgResponse "Series already tracked"
    Nothing -> unauthorizedResponse
