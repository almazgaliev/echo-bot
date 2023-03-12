{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Types.CallBackQuery where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Telegram.Bot.API.Wrapper.Types.Message as Message
import qualified Telegram.Bot.API.Wrapper.Types.User as User

data CallBackQuery = CallBackQuery
  { getCallbackId :: T.Text
  , getSender :: User.User
  , getMessage :: Maybe Message.Message
  , getData :: Maybe T.Text
  }
  deriving (Show)

instance Aeson.FromJSON CallBackQuery where
  parseJSON = Aeson.withObject "CallBackQuery" $ \v -> CallBackQuery <$> v .: "id" <*> v .: "from" <*> v .:? "message" <*> v .:? "data"