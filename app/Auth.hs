{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map as M
import Data.Password.Bcrypt
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.User
import Happstack.Server
import Response
import System.Environment
import Web.JWT

authRoutes conn =
  msum
    [ dir "register" $ register conn,
      dir "login" $ login conn,
      dir "logout" logout
    ]

register :: Connection -> ServerPart Response
register conn = do
  method POST
  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)

  formUsername <- look "username"
  formPassword <- look "password"
  formName <- look "name"

  checkUser <- liftIO $ getUserByUsername conn formUsername

  case checkUser of
    Nothing -> do
      hashedPassword <- hashPassword $ mkPassword $ T.pack formPassword

      liftIO $ addNewUser conn User {username = formUsername, password = T.unpack $ unPasswordHash hashedPassword, name = formName}

      ok $ msgResponse "Successfully registered"
    Just checkUser -> badRequest $ msgResponse "User already exists"

login :: Connection -> ServerPart Response
login conn = do
  method POST
  decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)

  formUsername <- look "username"
  formPassword <- look "password"

  checkUser <- liftIO $ getUserByUsername conn formUsername

  case checkUser of
    Just checkUser -> do
      let userPassword = mkPassword $ T.pack formPassword
          passwordHash = PasswordHash {unPasswordHash = T.pack $ password checkUser}
          passwordCheck = checkPassword userPassword passwordHash

      case passwordCheck of
        PasswordCheckSuccess -> do
          token <- liftIO $ generateToken (String $ T.pack $ username checkUser) (String $ T.pack $ name checkUser)
          let jwtCookie = mkCookie "startrack-jwt" $ T.unpack token
              cookieLife = MaxAge $ 7 * 24 * 3600

          addCookie cookieLife jwtCookie

          ok $ msgResponse "Password correct"
        PasswordCheckFail -> ok $ msgResponse "Password incorrect"
    Nothing -> badRequest $ msgResponse "User not found"

logout :: ServerPart Response
logout = do
  method POST

  expireCookie "startrack-jwt"

  ok $ msgResponse "Logged out"

readJwtSecret :: IO T.Text
readJwtSecret = do
  jwtSecretEnv <- getEnv "JWT_SECRET"
  return $ T.pack jwtSecretEnv

-- generateToken :: Value -> Value -> T.Text
generateToken username name = do
  jwtSecret <- readJwtSecret

  let claims = mempty {iss = stringOrURI "startrack-be", unregisteredClaims = ClaimsMap $ M.fromList [("username", username), ("name", name)]}
      key = hmacSecret jwtSecret

  return $ encodeSigned key mempty claims