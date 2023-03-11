{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Types.Markup (Markup (..), InlineKeyboardMarkup (..)) where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Telegram.Bot.API.Types.InlineKeyboardButton as InlineKeyboardButton

newtype InlineKeyboardMarkup = InlineKeyboardMarkup {getText :: [[InlineKeyboardButton.InlineKeyboardButton]]} deriving (Show)

data Markup
  = InlineKeyboard InlineKeyboardMarkup
  | ReplyKeyboardMarkup
  | ReplyKeyboardRemove
  | ForceReply
  deriving (Show)

instance Aeson.FromJSON Markup where
  parseJSON = Aeson.withObject "Markup" $ \v -> InlineKeyboard <$> v .: "reply_markup"

instance Aeson.FromJSON InlineKeyboardMarkup where
  parseJSON = Aeson.withObject "InlineKeyboardMarkup" $ \v -> InlineKeyboardMarkup <$> v .: "text"

instance Aeson.ToJSON Markup where
  toJSON (InlineKeyboard InlineKeyboardMarkup {getText = text}) = Aeson.object ["inline_keyboard" .= text]
  toJSON _ = error "Not Implemented"
