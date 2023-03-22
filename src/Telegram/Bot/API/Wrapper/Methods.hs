{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.API.Wrapper.Methods (getUpdates, sendMessage, answerCallbackQuery)
where



import qualified Data.Aeson as Aeson (eitherDecode)
import qualified Network.HTTP.Conduit as Conduit
import Network.HTTP.Types (Status (statusCode))
import qualified Telegram.Bot.API.Methods as Methods
import qualified Telegram.Bot.API.Types as Types
import qualified Telegram.Bot.API.Types.AnswerCallbackQueryParams as AnswerCallbackQueryParams
import qualified Telegram.Bot.API.Types.UpdateParams as UpdateParams (UpdateParams)
import qualified Telegram.Bot.API.Wrapper.Types.Message as Message
import qualified Telegram.Bot.API.Types.SendPhotoParams as Photo
import qualified Telegram.Bot.API.Wrapper.Types.Updates as Updates (Updates)

getUpdates ::
  Conduit.Manager ->
  Types.APIToken ->
  UpdateParams.UpdateParams ->
  IO (Either String Updates.Updates)
getUpdates manager token params = do
  response <- Methods.getUpdates manager token params
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
  IO (Either String ())
sendMessage manager token message = do
  _ <- Methods.sendMessage manager token message
  return $ Right () -- TODO handle errors

answerCallbackQuery :: Conduit.Manager -> Types.APIToken -> AnswerCallbackQueryParams.AnswerCallbackQueryParams -> IO ()
answerCallbackQuery m t p = do
  _ <- Methods.answerCallbackQuery m t p
  return () -- TODO handle errors

sendPhoto :: Conduit.Manager -> Types.APIToken -> Photo.SendPhotoParams -> IO ()
sendPhoto manager token photo = do
  _ <- Methods.sendPhoto manager token photo
  return () -- TODO handle errors
