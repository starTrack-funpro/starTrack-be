{-# LANGUAGE DeriveGeneric #-}

module Response where

import Data.Aeson
import GHC.Generics
import Happstack.Server

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