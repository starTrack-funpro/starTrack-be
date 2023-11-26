module SeriesTrack where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Time (TimeOfDay (TimeOfDay))
import Database.Chapter
import Database.Episode
import Database.PostgreSQL.Simple
import Database.Series
import qualified Database.Series as S
import Database.User
import Database.UserEpisode
import Database.UserSeries
import Happstack.Server
import JWT
import Utils

seriesTrackRoutes conn =
  msum
    [ nullDir >> getTrackedSeriesHandler conn,
      path $ \seriesId -> dir "episode" $ path $ \episodeNo -> trackEpisodeHandler conn seriesId episodeNo,
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

trackEpisodeHandler :: Connection -> Int -> Int -> ServerPartT IO Response
trackEpisodeHandler conn seriesId episodeNo = authenticate $ do
  method POST
  maybeUsername <- getUsernameFromJwt

  case maybeUsername of
    Just username -> do
      user <- liftIO $ getUserByUsername conn username
      fetchedSeries <- liftIO $ getSeriesById conn seriesId
      fetchedEpisode <- liftIO $ getEpisodeByNo conn seriesId episodeNo
      fetchedUserEpisode <- liftIO $ getUserEpisode conn username seriesId episodeNo

      case (user, fetchedSeries, fetchedEpisode, fetchedUserEpisode) of
        (Just user, Just _, Just _, Nothing) -> do
          let userEpisode = UserEpisode username seriesId episodeNo (TimeOfDay 0 0 0)
          liftIO $ addNewUserEpisode conn userEpisode
          ok $ msgResponse "Successfully track episode"
        (Nothing, _, _, _) -> unauthorizedResponse
        (_, Nothing, _, _) -> notFound $ msgResponse "Series not found"
        (_, _, Nothing, _) -> notFound $ msgResponse "Episode not found"
        (_, _, _, Just _) -> badRequest $ msgResponse "Episode already tracked"
    Nothing -> unauthorizedResponse
