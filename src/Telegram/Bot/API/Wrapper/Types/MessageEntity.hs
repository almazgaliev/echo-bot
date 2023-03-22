{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Bot.API.Wrapper.Types.MessageEntity (MessageEntity (..)) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as ATH
import Data.Aeson.Types ((.:))
import qualified Util

data MessageEntity = MessageEntity
  { getType :: String
  , getOffset :: Integer
  , getLength :: Integer
  }
  deriving (Show)

$(ATH.deriveJSON ATH.defaultOptions {Aeson.fieldLabelModifier = drop 3 . Util.toSnakeCase, Aeson.omitNothingFields = True} ''MessageEntity)
