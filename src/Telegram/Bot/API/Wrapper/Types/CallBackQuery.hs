{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Bot.API.Wrapper.Types.CallBackQuery where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as ATH
import qualified Data.Text as T
import qualified Telegram.Bot.API.Wrapper.Types.Message as Message
import qualified Telegram.Bot.API.Wrapper.Types.User as User
import qualified Util

data CallBackQuery = CallBackQuery
  { getId :: T.Text
  , getFrom :: User.User
  , getMessage :: Maybe Message.Message
  , getData :: Maybe T.Text
  }
  deriving (Show)

$(ATH.deriveJSON ATH.defaultOptions {Aeson.fieldLabelModifier = drop 3 . Util.toSnakeCase, Aeson.omitNothingFields = True} ''CallBackQuery)
