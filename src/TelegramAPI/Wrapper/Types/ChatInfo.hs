{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Wrapper.Types.ChatInfo (ChatInfo (..)) where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Word as Word

newtype ChatInfo = ChatInfo {getChatId :: Word.Word64} deriving (Show)

instance Aeson.FromJSON ChatInfo where
  parseJSON = Aeson.withObject "ChatInfo" $ \v -> ChatInfo <$> v .: "id"