{-# LANGUAGE OverloadedStrings #-}

module JWT where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Encoding
import Happstack.Server
import Happstack.Server.RqData
import System.Environment
import Utils (msgResponse, unauthorizedResponse)
import Web.JWT

readJwtSecret :: IO T.Text
readJwtSecret = do
  jwtSecretEnv <- getEnv "JWT_SECRET"
  return $ T.pack jwtSecretEnv

generateToken username role = do
  jwtSecret <- readJwtSecret

  let claims = mempty {iss = stringOrURI "startrack-be", unregisteredClaims = ClaimsMap $ M.fromList [("username", username), ("role", role)]}
      key = hmacSecret jwtSecret

  return $ encodeSigned key mempty claims

verifyToken token = do
  jwtSecret <- readJwtSecret

  let key = hmacSecret jwtSecret

  case decodeAndVerifySignature (toVerify key) token of
    Just verified -> do
      return $ Just verified
    Nothing -> return Nothing

authenticate :: ServerPartT IO Response -> ServerPartT IO Response
authenticate handler = do
  request <- askRq

  let jwtCookie = getJwtCookie request

  case jwtCookie of
    Just cookie -> do
      maybeVerified <- liftIO $ verifyToken (T.pack $ cookieValue cookie)
      case maybeVerified of
        Just verifiedJwt -> handler
        Nothing -> unauthorizedResponse
    Nothing -> unauthorizedResponse

getUsernameFromJwt :: ServerPartT IO (Maybe String)
getUsernameFromJwt = do
  request <- askRq

  let jwtCookie = getJwtCookie request

  case jwtCookie of
    Just cookie -> do
      maybeVerified <- liftIO $ verifyToken (T.pack $ cookieValue cookie)

      case maybeVerified of
        Just verified -> do
          let claimsMap = unClaimsMap $ unregisteredClaims $ claims verified

          case M.lookup "username" claimsMap of
            Just (String username) -> return $ Just $ T.unpack username
            Nothing -> return Nothing
        Nothing -> return Nothing
    Nothing -> return Nothing

getJwtCookie :: Request -> Maybe Cookie
getJwtCookie request = jwtCookie
  where
    cookies = rqCookies request
    jwtCookie = lookup "startrack-jwt" cookies