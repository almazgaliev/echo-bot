{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Types.InlineKeyboardButton (InlineKeyboardButton) where

import Data.Aeson (KeyValue ((.=)), object)
import qualified Data.Aeson as Aeson

newtype InlineKeyboardButton = InlineKeyboardButton {getText :: String} deriving (Show)

instance Aeson.ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton {getText = text}) = object ["text" .= text]
