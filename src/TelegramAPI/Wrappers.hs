{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Wrappers (getUpdates, UpdatesInfo (..), UpdateInfo (..), MessageInfo (..), ChatInfo (..))
where

import Data.Aeson (FromJSON (parseJSON), eitherDecode, withObject, (.:))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, Response (responseBody, responseStatus))
import Network.HTTP.Types (Status (..))
import qualified TelegramAPI.Raw as TAPI

newtype ChatInfo = ChatInfo {id :: Integer} deriving (Show, Generic, FromJSON)

data MessageInfo = MessageInfo {chatInfo :: ChatInfo, text :: String} deriving (Show)

instance FromJSON MessageInfo where
  parseJSON = withObject "MessageInfo" $ \v -> MessageInfo <$> v .: "chat" <*> v .: "text"

data UpdateInfo = UpdateInfo {updateId :: Integer, message :: MessageInfo} deriving (Show)

instance FromJSON UpdateInfo where
  parseJSON = withObject "UpdateInfo" $ \v -> UpdateInfo <$> v .: "update_id" <*> v .: "message"

data UpdatesInfo = UpdatesInfo {ok :: Bool, result :: [UpdateInfo]} deriving (Show, Generic, FromJSON)

getUpdates :: Manager -> String -> IO (Either String UpdatesInfo)
getUpdates manager token = do
  response <- TAPI.getUpdates manager token
  let code = statusCode . responseStatus $ response
  let body = responseBody response
  return $
    if code == 200
      then eitherDecode body
      else Left $ "response code: " ++ show code
