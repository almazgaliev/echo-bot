{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Bot.API.Wrapper.Types.InlineKeyboardButton (InlineKeyboardButton, mkInlineCallBackButton) where

import Data.Aeson ((.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Aeson.TH as ATH
import qualified Util

data InlineKeyboardButton = InlineKeyboardButton {getText :: Maybe T.Text, getCallbackData :: Maybe T.Text} deriving (Show)

$(ATH.deriveJSON ATH.defaultOptions {Aeson.fieldLabelModifier = drop 3 . Util.toSnakeCase, Aeson.omitNothingFields = True} ''InlineKeyboardButton)


mkInlineCallBackButton :: T.Text -> T.Text -> InlineKeyboardButton
mkInlineCallBackButton label data' =
  InlineKeyboardButton
    { getText = pure label
    , getCallbackData = pure data'
    }