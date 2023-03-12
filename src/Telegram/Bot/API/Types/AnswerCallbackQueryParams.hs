{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Types.AnswerCallbackQueryParams (AnswerCallbackQueryParams (..)) where

import Data.Aeson (KeyValue ((.=)), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

data AnswerCallbackQueryParams = AnswerCallbackQueryParams
  { getCallbackQueryId :: T.Text
  , getText :: Maybe T.Text
  }
  deriving (Show)

instance Aeson.FromJSON AnswerCallbackQueryParams where
  parseJSON = Aeson.withObject "AnswerCallbackQueryParams" $ \v -> AnswerCallbackQueryParams <$> v .: "callback_query_id" <*> v .:? "text"

instance Aeson.ToJSON AnswerCallbackQueryParams where
  toJSON (AnswerCallbackQueryParams {getCallbackQueryId = queryId, getText = text}) = Aeson.object ["callback_query_id" .= queryId, "text" .= text]