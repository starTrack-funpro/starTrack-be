{-# LANGUAGE OverloadedStrings #-}

module JWT where

import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding
import Happstack.Server
import Happstack.Server.RqData
import System.Environment
import Utils (msgResponse)
import Web.JWT

readJwtSecret :: IO T.Text
readJwtSecret = do
  jwtSecretEnv <- getEnv "JWT_SECRET"
  return $ T.pack jwtSecretEnv

generateToken username name = do
  jwtSecret <- readJwtSecret

  let claims = mempty {iss = stringOrURI "startrack-be", unregisteredClaims = ClaimsMap $ M.fromList [("username", username), ("name", name)]}
      key = hmacSecret jwtSecret

  return $ encodeSigned key mempty claims

verifyToken token = do
  jwtSecret <- readJwtSecret

  let key = hmacSecret jwtSecret

  case decodeAndVerifySignature (toVerify key) (T.drop 7 token) of
    Just verified -> do
      return True
    Nothing -> return False

authenticate :: ServerPartT IO Response -> ServerPartT IO Response
authenticate handler = do
  request <- askRq
  let headers = rqHeaders request
      authHeader = M.lookup "authorization" headers

  case authHeader of
    Just headerPair -> do
      verified <- liftIO $ verifyToken (decodeUtf8 $ head $ hValue headerPair)
      if verified then handler else unauthorizedResponse
    Nothing -> unauthorizedResponse
  where
    unauthorizedResponse = unauthorized $ msgResponse "Unauthorized Access"