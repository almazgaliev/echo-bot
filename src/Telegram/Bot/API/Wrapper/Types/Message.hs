{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Bot.API.Wrapper.Types.Message (Message (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as ATH
import qualified Util

import qualified Data.Text as T
import qualified Telegram.Bot.API.Wrapper.Types.Chat as Chat
import qualified Telegram.Bot.API.Wrapper.Types.Markup as Markup
import qualified Telegram.Bot.API.Wrapper.Types.MessageEntity as MessageEntity
import qualified Telegram.Bot.API.Wrapper.Types.User as User

data Message = Message
  { getChat :: Chat.Chat
  , getFrom :: Maybe User.User
  , getText :: Maybe T.Text
  , getMarkup :: Maybe Markup.Markup
  , getEntities :: Maybe [MessageEntity.MessageEntity]
  }
  deriving (Show)

$(ATH.deriveJSON ATH.defaultOptions {Aeson.fieldLabelModifier = drop 3 . Util.toSnakeCase, Aeson.omitNothingFields = True} ''Message)
