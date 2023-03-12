{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Types.Chat (Chat (..)) where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Word as Word

newtype Chat = Chat {getChatId :: Word.Word64} deriving (Show)

instance Aeson.FromJSON Chat where
  parseJSON = Aeson.withObject "Chat" $ \v -> Chat <$> v .: "id"