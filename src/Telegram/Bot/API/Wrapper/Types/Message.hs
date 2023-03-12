{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Types.Message (Message (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson

-- import qualified Data.Text as T -- TODO

import qualified Data.Text as T
import qualified Telegram.Bot.API.Wrapper.Types.Chat as Chat
import qualified Telegram.Bot.API.Wrapper.Types.Markup as Markup
import qualified Telegram.Bot.API.Wrapper.Types.MessageEntity as MessageEntity
import qualified Telegram.Bot.API.Wrapper.Types.User as User

data Message = Message
  { getChat :: Chat.Chat
  , getSender :: Maybe User.User
  , getText :: Maybe T.Text
  , getMarkup :: Maybe Markup.Markup
  , getEntities :: Maybe [MessageEntity.MessageEntity]
  }
  deriving (Show)

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \v ->
    Message <$> v .: "chat" <*> v .:? "from" <*> v .:? "text" <*> v .:? "reply_markup" <*> v .:? "entities"
