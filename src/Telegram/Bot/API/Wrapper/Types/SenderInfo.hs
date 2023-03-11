{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Types.SenderInfo (SenderInfo (..)) where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Word as Word

data SenderInfo = SenderInfo
  { getUserId :: Word.Word64
  , getFirstName :: String
  , getUserName :: String
  }
  deriving (Show)

instance Aeson.FromJSON SenderInfo where
  parseJSON = Aeson.withObject "SenderInfo" $ \v ->
    SenderInfo <$> v .: "id" <*> v .: "first_name" <*> v .: "username"