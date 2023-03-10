{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Types.Markup (Markup (..)) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified TelegramAPI.Types.InlineKeyboardButton as InlineKeyboardButton

data Markup
  = InlineKeyboardMarkup {getText :: [[InlineKeyboardButton.InlineKeyboardButton]]} -- add optional types
  | ReplyKeyboardMarkup
  | ReplyKeyboardRemove
  | ForceReply

instance Aeson.ToJSON Markup where
  toJSON (InlineKeyboardMarkup {getText = text}) = Aeson.object ["inline_keyboard" .= text]
  toJSON _ = error "Not Implemented"
