{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Types.Markup (Markup (..)) where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Telegram.Bot.API.Wrapper.Types.InlineKeyboardButton as InlineKeyboardButton

data Markup
  = InlineKeyboard [[InlineKeyboardButton.InlineKeyboardButton]]
  | ReplyKeyboardMarkup
  | ReplyKeyboardRemove
  | ForceReply
  deriving (Show)

instance Aeson.FromJSON Markup where
  parseJSON = Aeson.withObject "Markup" $ \v -> InlineKeyboard <$> v .: "inline_keyboard"

instance Aeson.ToJSON Markup where
  toJSON (InlineKeyboard arr) = Aeson.object ["inline_keyboard" .= arr]
  toJSON _ = error "Not Implemented"
