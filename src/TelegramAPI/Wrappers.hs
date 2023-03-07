{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Wrappers (getUpdates, SenderInfo (..), UpdatesInfo (..), UpdateInfo (..), MessageInfo (..), ChatInfo (..))
where

import Data.Aeson ((.:), (.:?))
import qualified Data.Aeson as Aeson (FromJSON (parseJSON), eitherDecode, withObject)
import qualified Data.Text as T
import qualified Data.Word as Word (Word64)
import qualified Network.HTTP.Conduit as Conduit
import Network.HTTP.Types (Status (statusCode))
import qualified TelegramAPI

newtype ChatInfo = ChatInfo {getChatId :: Word.Word64} deriving (Show)

instance Aeson.FromJSON ChatInfo where
  parseJSON = Aeson.withObject "ChatInfo" $ \v -> ChatInfo <$> v .: "id"

data SenderInfo = SenderInfo
  { getUserId :: Word.Word64
  , getFirstName :: String
  , getUserName :: String
  }
  deriving (Show)

instance Aeson.FromJSON SenderInfo where
  parseJSON = Aeson.withObject "SenderInfo" $ \v ->
    SenderInfo <$> v .: "id" <*> v .: "first_name" <*> v .: "username"

data MessageInfo = MessageInfo {getChatInfo :: ChatInfo, getSender :: SenderInfo, getText :: Maybe T.Text} deriving (Show)

instance Aeson.FromJSON MessageInfo where
  parseJSON = Aeson.withObject "MessageInfo" $ \v ->
    MessageInfo <$> v .: "chat" <*> v .: "from" <*> v .:? "text"

data UpdateInfo = UpdateInfo {getUpdateId :: Word.Word64, getMessage :: Maybe MessageInfo} deriving (Show)

instance Aeson.FromJSON UpdateInfo where
  parseJSON = Aeson.withObject "UpdateInfo" $ \v -> UpdateInfo <$> v .: "update_id" <*> v .:? "message"

data UpdatesInfo = UpdatesInfo {ok :: Bool, getResult :: [UpdateInfo]} deriving (Show)

instance Aeson.FromJSON UpdatesInfo where
  parseJSON = Aeson.withObject "UpdatesInfo" $ \v -> UpdatesInfo <$> v .: "ok" <*> v .: "result"

getUpdates :: Conduit.Manager -> String -> TelegramAPI.UpdateParams -> IO (Either String UpdatesInfo)
getUpdates manager token params = do
  response <- TelegramAPI.getUpdates manager token params
  let code = statusCode . Conduit.responseStatus $ response
  let body = Conduit.responseBody response
  return $
    if code == 200
      then Aeson.eitherDecode body
      else Left $ "response code: " ++ show code
