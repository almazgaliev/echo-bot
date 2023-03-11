{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper (getUpdates, sendMessage)
where

import qualified Data.Aeson as Aeson (eitherDecode)
import qualified Data.Word as Word
import qualified Network.HTTP.Conduit as Conduit
import Network.HTTP.Types (Status (statusCode))
import qualified Telegram.Bot.API as TelegramAPI
import qualified Telegram.Bot.API.Types as Types
import qualified Telegram.Bot.API.Types.Message as Message
import qualified Telegram.Bot.API.Types.UpdateParams as UpdateParams (UpdateParams)
import qualified Telegram.Bot.API.Wrapper.Types.Updates as Updates (UpdatesInfo)

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
  Word.Word64 ->
  Message.Message ->
  IO (Either String ())
sendMessage manager token chatId message = do
  _ <- TelegramAPI.sendMessage manager token chatId message
  return $ Right () -- TODO handle errors