{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Types.Updates (Updates (..), Update (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Word as Word
import qualified Telegram.Bot.API.Wrapper.Types.CallBackQuery as CallBackQuery
import qualified Telegram.Bot.API.Wrapper.Types.Message as Message

data Update = Update
  { getUpdateId :: Word.Word64
  , getMessage :: Maybe Message.Message
  , getCallBack :: Maybe CallBackQuery.CallBackQuery
  }
  deriving (Show)

instance Aeson.FromJSON Update where
  parseJSON = Aeson.withObject "Update" $ \v -> Update <$> v .: "update_id" <*> v .:? "message" <*> v .:? "callback_query"

data Updates = Updates {ok :: Bool, getResult :: [Update]} deriving (Show)

instance Aeson.FromJSON Updates where
  parseJSON = Aeson.withObject "Updates" $ \v -> Updates <$> v .: "ok" <*> v .: "result"
