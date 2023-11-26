module ChapterTrack where

import Control.Monad
import Control.Monad.IO.Class
import Database.Chapter
import Database.PostgreSQL.Simple
import Database.Series
import Database.User
import Database.UserChapter
import Happstack.Server
import JWT
import Utils

chapterTrackRoutes conn seriesId =
  msum
    [ path $ \episodeNo -> trackChapterHandler conn seriesId episodeNo
    ]

trackChapterHandler :: Connection -> Int -> Int -> ServerPartT IO Response
trackChapterHandler conn seriesId chapterNo = authenticate $ do
  method POST
  maybeUsername <- getUsernameFromJwt

  case maybeUsername of
    Just username -> do
      user <- liftIO $ getUserByUsername conn username
      fetchedSeries <- liftIO $ getSeriesById conn seriesId
      fetchedChapter <- liftIO $ getChapterByNo conn seriesId chapterNo
      fetchedUserChapter <- liftIO $ getUserChapter conn username seriesId chapterNo

      case (user, fetchedSeries, fetchedChapter, fetchedUserChapter) of
        (Just user, Just _, Just chapter, Nothing) -> do
          let userChapter = UserChapter username seriesId chapterNo (pageFrom chapter)
          liftIO $ addNewUserChapter conn userChapter
          ok $ msgResponse "Successfully track chapter"
        (Nothing, _, _, _) -> unauthorizedResponse
        (_, Nothing, _, _) -> notFound $ msgResponse "Series not found"
        (_, _, Nothing, _) -> notFound $ msgResponse "Chapter not found"
        (_, _, _, Just _) -> badRequest $ msgResponse "Chapter already tracked"
    Nothing -> unauthorizedResponse