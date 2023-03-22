{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Bot.API.Wrapper.Types.Chat (Chat (..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as ATH
import qualified Data.Word as Word
import qualified Util

newtype Chat = Chat {getId :: Word.Word64} deriving (Show)

$(ATH.deriveJSON ATH.defaultOptions {Aeson.fieldLabelModifier = drop 3 . Util.toSnakeCase, Aeson.omitNothingFields = True} ''Chat)
