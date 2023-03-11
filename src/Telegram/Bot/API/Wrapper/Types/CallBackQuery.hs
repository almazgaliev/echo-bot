{-# LANGUAGE OverloadedStrings #-}
module Telegram.Bot.API.Wrapper.Types.CallBackQuery where

import qualified Telegram.Bot.API.Types.Message as Message
import qualified Telegram.Bot.API.Wrapper.Types.SenderInfo as SenderInfo
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.:?))

data CallBackQuery = CallBackQuery
  { getCallbackId :: String
  , getSender :: SenderInfo.SenderInfo
  , getMessage :: Maybe Message.Message
  }
  deriving (Show)

instance Aeson.FromJSON CallBackQuery where
  parseJSON = Aeson.withObject "CallBackQuery" $ \v -> CallBackQuery <$> v .: "id" <*> v .: "from" <*> v .:? "message"