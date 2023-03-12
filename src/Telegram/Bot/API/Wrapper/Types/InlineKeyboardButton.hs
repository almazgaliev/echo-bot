{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Types.InlineKeyboardButton (InlineKeyboardButton, mkInlineCallBackButton) where

import Data.Aeson ((.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

data InlineKeyboardButton = InlineKeyboardButton {getText :: Maybe T.Text, callback :: Maybe T.Text} deriving (Show)

instance Aeson.FromJSON InlineKeyboardButton where
  parseJSON = Aeson.withObject "InlineKeyboardButton" $ \v -> InlineKeyboardButton <$> v .:? "text" <*> v .:? "callback_data"
instance Aeson.ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton {getText = t, callback = cb}) = Aeson.object ["text" .= t, "callback_data" .= cb]

mkInlineCallBackButton :: T.Text -> T.Text -> InlineKeyboardButton
mkInlineCallBackButton label data' =
  InlineKeyboardButton
    { getText = pure label
    , callback = pure data'
    }