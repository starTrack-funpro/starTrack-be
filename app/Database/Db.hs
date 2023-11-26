module Database.Db where

import qualified Data.Text as T
import Data.Text.Encoding
import Database.PostgreSQL.Simple
import System.Environment

readDbUrl = do
  urlEnv <- getEnv "DATABASE_URL"
  return $ encodeUtf8 $ T.pack urlEnv

queryOne conn queryString params = do
  fetched <- query conn queryString params

  if null fetched
    then return Nothing
    else return $ Just $ head fetched