{-# LANGUAGE DeriveGeneric #-}

module Utils where

import Data.Aeson
import Data.Maybe
import Data.Time
import GHC.Generics
import Happstack.Server

-- Response Utils
newtype Message = Message
  { message :: String
  }
  deriving (Generic)

instance ToJSON Message where
  toEncoding = genericToEncoding defaultOptions

-- Response Config
defaultHeaders =
  setHeader "Content-Type" "application/json"
    . setHeader "Access-Control-Allow-Origin" "http://localhost:3000"
    . setHeader "Access-Control-Allow-Credentials" "true"

defaultResponse content = defaultHeaders $ toResponse content

msgResponse :: String -> Response
msgResponse msg = defaultResponse $ encode Message {message = msg}

unauthorizedResponse :: ServerPartT IO Response
unauthorizedResponse = unauthorized $ msgResponse "Unauthorized Access"

-- Body Decoder Utils
decodeRequestBody :: ServerPartT IO a -> ServerPartT IO a
decodeRequestBody handler = decodeBody bodyPolicy >> handler
  where
    bodyPolicy = defaultBodyPolicy "/tmp/" 4096 4096 4096

-- String to TimeOfDay parser
parseDuration :: String -> TimeOfDay
parseDuration str = fromMaybe (TimeOfDay 0 0 0) $ parseTimeM True defaultTimeLocale "%H:%M:%S" str