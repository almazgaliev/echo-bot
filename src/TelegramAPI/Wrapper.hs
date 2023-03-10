{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Wrapper (getUpdates, sendMessage)
where

import qualified Data.Aeson as Aeson (eitherDecode)
import qualified Data.Word as Word
import qualified Network.HTTP.Conduit as Conduit
import Network.HTTP.Types (Status (statusCode))
import qualified TelegramAPI
import qualified TelegramAPI.Types as Types
import qualified TelegramAPI.Types.Message as Message
import qualified TelegramAPI.Types.UpdateParams as UpdateParams (UpdateParams)
import qualified TelegramAPI.Wrapper.Types.Updates as Updates (UpdatesInfo)

getUpdates ::
  Conduit.Manager ->
  Types.APIToken ->
  UpdateParams.UpdateParams ->
  IO (Either String Updates.UpdatesInfo)
getUpdates manager token params = do
  response <- TelegramAPI.getUpdates manager token params
  let code = statusCode . Conduit.responseStatus $ response
  let body = Conduit.responseBody response
  return $
    if code == 200
      then Aeson.eitherDecode body
      else Left $ "response code: " ++ show code

sendMessage ::
  Conduit.Manager ->
  Types.APIToken ->
  Message.Message ->
  Word.Word64 ->
  IO (Either String ())
sendMessage manager token message chatId = do
  _ <- TelegramAPI.sendMessage manager token message chatId
  return $ Right ()