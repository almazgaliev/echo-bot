{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Types.SendPhotoParams (SendPhotoParams (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson

import qualified Telegram.Bot.API.Wrapper.Types.MessageEntity as MessageEntity

data SendPhotoParams = Photo
  { getChatId :: String
  , getPhoto :: String
  , getCaptionEntities :: Maybe [MessageEntity.MessageEntity]
  , getCaption :: Maybe String
  -- , getText :: Maybe T.Text
  -- , getMarkup :: Maybe Markup.Markup
  -- , getEntities :: Maybe [MessageEntity.MessageEntity]
  }
  deriving (Show)

instance Aeson.FromJSON SendPhotoParams where
  parseJSON = Aeson.withObject "Message" $ \v ->
    Photo <$> v .: "chat_id" <*> v .: "photo" <*> v .:? "caption_entities" <*> v .:? "caption"
