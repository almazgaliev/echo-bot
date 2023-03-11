{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Types.Message (Message (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Telegram.Bot.API.Types.Markup as Markup

data Message = Message
  { getText :: Maybe String
  , getMarkup :: Maybe Markup.Markup
  }
  deriving (Show)

instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \v -> Message <$> v .: "text" <*> v .:? "reply_markup"
