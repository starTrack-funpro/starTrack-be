module EpisodeTrack where

import Control.Monad
import Control.Monad.IO.Class
import Data.Time
import Database.Episode
import Database.PostgreSQL.Simple
import Database.Series
import Database.User
import Database.UserEpisode
import Happstack.Server
import JWT
import Utils

episodeTrackRoutes conn seriesId =
  msum
    [ path $ \episodeNo -> trackEpisodeHandler conn seriesId episodeNo,
      path $ \episodeNo -> updateTrackHandler conn seriesId episodeNo
    ]

trackEpisodeHandler :: Connection -> Int -> Int -> ServerPartT IO Response
trackEpisodeHandler conn seriesId episodeNo = authenticate $ decodeRequestBody $ do
  method POST
  maybeUsername <- getUsernameFromJwt

  formLastWatchTimeStr <- look "lastWatchTime"
  let formLastWatchTime = parseDuration formLastWatchTimeStr

  case maybeUsername of
    Just username -> do
      user <- liftIO $ getUserByUsername conn username
      fetchedSeries <- liftIO $ getSeriesById conn seriesId
      fetchedEpisode <- liftIO $ getEpisodeByNo conn seriesId episodeNo
      fetchedUserEpisode <- liftIO $ getUserEpisode conn username seriesId episodeNo

      case (user, fetchedSeries, fetchedEpisode, fetchedUserEpisode) of
        (Just user, Just _, Just _, Nothing) -> do
          let userEpisode = UserEpisode username seriesId episodeNo formLastWatchTime
          liftIO $ addNewUserEpisode conn userEpisode
          ok $ msgResponse "Successfully track episode"
        (Nothing, _, _, _) -> unauthorizedResponse
        (_, Nothing, _, _) -> notFound $ msgResponse "Series not found"
        (_, _, Nothing, _) -> notFound $ msgResponse "Episode not found"
        (_, _, _, Just _) -> badRequest $ msgResponse "Episode already tracked"
    Nothing -> unauthorizedResponse

updateTrackHandler :: Connection -> Int -> Int -> ServerPartT IO Response
updateTrackHandler conn seriesId episodeNo = authenticate $ decodeRequestBody $ do
  method PATCH
  maybeUsername <- getUsernameFromJwt

  formLastWatchTimeStr <- look "lastWatchTime"
  let formLastWatchTime = parseDuration formLastWatchTimeStr

  case maybeUsername of
    Just username -> do
      user <- liftIO $ getUserByUsername conn username
      fetchedSeries <- liftIO $ getSeriesById conn seriesId
      fetchedEpisode <- liftIO $ getEpisodeByNo conn seriesId episodeNo
      fetchedUserEpisode <- liftIO $ getUserEpisode conn username seriesId episodeNo

      case (user, fetchedSeries, fetchedEpisode, fetchedUserEpisode) of
        (Just user, Just _, Just _, Just _) -> do
          liftIO $ updateLastWatchTime conn username seriesId episodeNo formLastWatchTime
          ok $ msgResponse "Successfully update episode track"
        (Nothing, _, _, _) -> unauthorizedResponse
        (_, Nothing, _, _) -> notFound $ msgResponse "Series not found"
        (_, _, Nothing, _) -> notFound $ msgResponse "Episode not found"
        (_, _, _, Nothing) -> badRequest $ msgResponse "Episode not tracked"
    Nothing -> unauthorizedResponse
