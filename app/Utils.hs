{-# LANGUAGE DeriveGeneric #-}

module Utils where

import Data.Aeson
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
defaultResponse content = setHeader "Content-Type" "application/json" $ toResponse content

msgResponse :: String -> Response
msgResponse msg = defaultResponse $ encode Message {message = msg}

-- Body Decoder Utils
decodeRequestBody :: ServerPartT IO a -> ServerPartT IO a
decodeRequestBody handler = decodeBody bodyPolicy >> handler
  where
    bodyPolicy = defaultBodyPolicy "/tmp/" 4096 4096 4096