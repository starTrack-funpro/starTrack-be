{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Map as M
import Data.Password.Bcrypt
import qualified Data.Text as T
import Database.PostgreSQL.Simple
import Database.User as U
import GHC.Generics
import Happstack.Server
import JWT
import System.Environment
import Utils
import Web.JWT

authRoutes conn =
  msum
    [ dir "register" $ register conn,
      dir "login" $ login conn,
      dir "logout" logout,
      dir "protected" $ protected conn
    ]

register :: Connection -> ServerPart Response
register conn = decodeRequestBody $ do
  method POST

  formUsername <- look "username"
  formPassword <- look "password"
  formName <- look "name"

  checkUser <- liftIO $ getUserByUsername conn formUsername

  case checkUser of
    Nothing -> do
      hashedPassword <- hashPassword $ mkPassword $ T.pack formPassword

      liftIO $ addNewUser conn User {U.username = formUsername, U.password = T.unpack $ unPasswordHash hashedPassword, U.name = formName}

      ok $ msgResponse "Successfully registered"
    Just checkUser -> badRequest $ msgResponse "User already exists"

login :: Connection -> ServerPart Response
login conn = decodeRequestBody $ do
  method POST

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
          token <- liftIO $ generateToken (String $ T.pack $ U.username checkUser)
          let jwtCookie = createCookie "startrack-jwt" $ T.unpack token
              cookieLife = MaxAge $ 7 * 24 * 3600

          addCookie cookieLife jwtCookie

          ok $ msgResponse "Successfully logged in"
        PasswordCheckFail -> badRequest $ msgResponse "Password incorrect"
    Nothing -> badRequest $ msgResponse "User not found"

logout :: ServerPart Response
logout = do
  method POST

  expireCookie "startrack-jwt"

  ok $ msgResponse "Logged out"

protected :: Connection -> ServerPart Response
protected conn = authenticate $ do
  method GET

  maybeUsername <- getUsernameFromJwt

  case maybeUsername of
    Just username -> do
      fetchedUser <- liftIO $ getUserByUsername conn username
      case fetchedUser of
        Just user ->
          ok $ defaultResponse $ encode $ UserInfo (U.username user) (U.name user)
        Nothing -> unauthorizedResponse
    Nothing -> do
      expireCookie "startrack-jwt"
      unauthorizedResponse

createCookie key val = Cookie "1" "/" "" key val False True SameSiteNoValue

data UserInfo = UserInfo
  { username :: String,
    name :: String
  }
  deriving (Generic, Show, ToJSON)