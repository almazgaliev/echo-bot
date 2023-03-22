{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Bot.API.Wrapper.Types.User (User (..)) where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Word as Word
import qualified Data.Aeson.TH as ATH
import qualified Util

data User = User
  { getId :: Word.Word64
  , getFirstName :: T.Text
  , getUsername :: T.Text
  }
  deriving (Show)


$(ATH.deriveJSON ATH.defaultOptions{Aeson.fieldLabelModifier = drop 3 . Util.toSnakeCase, Aeson.omitNothingFields = True} ''User)