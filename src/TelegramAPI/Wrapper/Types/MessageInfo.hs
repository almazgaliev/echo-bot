{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Wrapper.Types.MessageInfo (MessageInfo (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified TelegramAPI.Wrapper.Types.ChatInfo as ChatInfo
import qualified TelegramAPI.Wrapper.Types.SenderInfo as SenderInfo

data MessageInfo = MessageInfo {getChatInfo :: ChatInfo.ChatInfo, getSender :: SenderInfo.SenderInfo, getText :: Maybe T.Text} deriving (Show)

instance Aeson.FromJSON MessageInfo where
  parseJSON = Aeson.withObject "MessageInfo" $ \v ->
    MessageInfo <$> v .: "chat" <*> v .: "from" <*> v .:? "text"
