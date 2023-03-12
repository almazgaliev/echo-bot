{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Types.User (User (..)) where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Word as Word

data User = User
  { getUserId :: Word.Word64
  , getFirstName :: T.Text
  , getUserName :: T.Text
  }
  deriving (Show)

instance Aeson.FromJSON User where
  parseJSON = Aeson.withObject "User" $ \v ->
    User <$> v .: "id" <*> v .: "first_name" <*> v .: "username"