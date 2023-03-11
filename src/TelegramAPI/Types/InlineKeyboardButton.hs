{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Types.InlineKeyboardButton (InlineKeyboardButton, mkInlineCallBackButton) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson

data RequiredField = CallBack String | Url String deriving (Show)

data InlineKeyboardButton = InlineKeyboardButton {getText :: Maybe String, requieredField :: RequiredField} deriving (Show)

instance Aeson.ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton {getText = text, requieredField = (CallBack callback)}) = Aeson.object ["text" .= text, "callback_data" .= callback]
  toJSON _ = error "Not Implemented"

mkInlineCallBackButton :: String -> String -> InlineKeyboardButton
mkInlineCallBackButton label data' =
  InlineKeyboardButton
    { getText = pure label
    , requieredField = CallBack data'
    }