module Database.Db where

import qualified Data.Text as T
import Data.Text.Encoding
import System.Environment

readDbUrl = do
  urlEnv <- getEnv "DATABASE_URL"
  return $ encodeUtf8 $ T.pack urlEnv