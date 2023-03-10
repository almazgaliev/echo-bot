{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Wrapper.Types.Updates (UpdatesInfo (..), UpdateInfo (..)) where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Word as Word
import qualified TelegramAPI.Wrapper.Types.MessageInfo as MessageInfo

data UpdateInfo = UpdateInfo {getUpdateId :: Word.Word64, getMessage :: Maybe MessageInfo.MessageInfo} deriving (Show)

instance Aeson.FromJSON UpdateInfo where
  parseJSON = Aeson.withObject "UpdateInfo" $ \v -> UpdateInfo <$> v .: "update_id" <*> v .:? "message"

data UpdatesInfo = UpdatesInfo {ok :: Bool, getResult :: [UpdateInfo]} deriving (Show)

instance Aeson.FromJSON UpdatesInfo where
  parseJSON = Aeson.withObject "UpdatesInfo" $ \v -> UpdatesInfo <$> v .: "ok" <*> v .: "result"
