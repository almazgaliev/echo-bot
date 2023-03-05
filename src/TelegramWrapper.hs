{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TelegramWrapper (getUpdates, UpdatesInfo (..), UpdateInfo (..), MessageInfo (..), ChatInfo (..))
where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Response (responseBody, responseStatus))
import Network.HTTP.Types (Status (..))
import qualified TelegramAPI as TAPI

newtype ChatInfo = ChatInfo {id :: Int} deriving (Show)

instance FromJSON ChatInfo where
  parseJSON = withObject "ChatInfo" $ \v ->
    ChatInfo
      <$> v
      .: "id"

data MessageInfo = MessageInfo {chatInfo :: ChatInfo, text :: String} deriving (Show)

instance FromJSON MessageInfo where
  parseJSON = withObject "MessageInfo" $ \v ->
    MessageInfo
      <$> v
      .: "chat"
      <*> v
      .: "text"

data UpdateInfo = UpdateInfo {updateId :: Int, message :: MessageInfo} deriving (Show)

instance FromJSON UpdateInfo where
  parseJSON = withObject "UpdateInfo" $ \v ->
    UpdateInfo
      <$> v
      .: "update_id"
      <*> v
      .: "message"

data UpdatesInfo = UpdatesInfo {ok :: Bool, result :: [UpdateInfo]} deriving (Show, Generic, FromJSON)

getUpdates :: Manager -> String -> IO (Either String UpdatesInfo)
getUpdates manager token = do
  response <- TAPI.getUpdates manager token
  let code = statusCode . responseStatus $ response
  return $
    if code == 200
      then eitherDecode . responseBody $ response
      else Left $ "response code is " ++ show code
