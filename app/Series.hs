module Series where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.Series
import Happstack.Server
import Response

seriesRoutes conn =
  msum
    [ nullDir >> allSeries conn
    ]

allSeries :: Connection -> ServerPart Response
allSeries conn = do
  method GET

  series <- liftIO $ getAllSeries conn

  ok $ defaultResponse $ encode series