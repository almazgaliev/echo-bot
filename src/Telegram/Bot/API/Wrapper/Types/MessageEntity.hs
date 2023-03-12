{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Types.MessageEntity (MessageEntity (..)) where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types ((.:))

data MessageEntity = MessageEntity
  { getType :: String
  , getOffset :: Integer
  , getLength :: Integer
  }
  deriving (Show)

instance Aeson.ToJSON MessageEntity where
  toJSON (MessageEntity t o l) =
    Aeson.object
      [ "type" .= t
      , "offset" .= o
      , "length" .= l
      ]

instance Aeson.FromJSON MessageEntity where
  parseJSON = Aeson.withObject "MessageEntity" $ \v ->
    MessageEntity
      <$> v
      .: "type"
      <*> v
      .: "offset"
      <*> v
      .: "length"