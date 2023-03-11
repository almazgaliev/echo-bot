{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Types.InlineKeyboardButton (InlineKeyboardButton, mkInlineCallBackButton) where

import Data.Aeson ((.:?), (.=))
import qualified Data.Aeson as Aeson

data InlineKeyboardButton = InlineKeyboardButton {getText :: Maybe String, callback :: Maybe String} deriving (Show)

instance Aeson.FromJSON InlineKeyboardButton where
  parseJSON = Aeson.withObject "InlineKeyboardButton" $ \v -> InlineKeyboardButton <$> v .:? "text" <*> v .:? "callback_data"
instance Aeson.ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton {getText = t, callback = cb}) = Aeson.object ["text" .= t, "callback_data" .= cb]

mkInlineCallBackButton :: String -> String -> InlineKeyboardButton
mkInlineCallBackButton label data' =
  InlineKeyboardButton
    { getText = pure label
    , callback = pure data'
    }