{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Telegram.Bot.API.Types.AnswerCallbackQueryParams (AnswerCallbackQueryParams (..)) where

import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Aeson.TH as ATH
import qualified Util

data AnswerCallbackQueryParams = AnswerCallbackQueryParams
  { getCallbackQueryId :: T.Text
  , getText :: Maybe T.Text
  }
  deriving (Show)

$(ATH.deriveJSON ATH.defaultOptions {Aeson.fieldLabelModifier = drop 3 . Util.toSnakeCase, Aeson.omitNothingFields = True} ''AnswerCallbackQueryParams)
