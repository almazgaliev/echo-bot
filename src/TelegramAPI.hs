{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TelegramAPI (
  sendMessage,
  getUpdates,
) where

import qualified Data.ByteString.Internal as BS (ByteString, packChars)
import qualified Network.HTTP.Conduit as Conduit (Manager, Response, httpLbs, parseRequest)

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Word as Word (Word64)
import qualified Network.HTTP.Simple as Simple (
  setRequestBodyURLEncoded,
  setRequestMethod,
 )
import qualified TelegramAPI.Types as Types
import qualified TelegramAPI.Types.Message as Message
import qualified TelegramAPI.Types.UpdateParams as UpdateParams

paramsToBody :: UpdateParams.UpdateParams -> [(BS.ByteString, BS.ByteString)]
paramsToBody params = [("offset", BS.packChars . show $ UpdateParams.getOffset params)]

getReplyMarkup' :: Message.Message -> Maybe (BS.ByteString, BS.ByteString)
getReplyMarkup' message = ("reply_markup",) . LBS.toStrict . Aeson.encode <$> Message.getMarkup message

getText' :: Message.Message -> Maybe (BS.ByteString, BS.ByteString)
getText' message = ("text",) . T.encodeUtf8 . T.pack <$> Message.getText message

apiURL :: String
apiURL = "https://api.telegram.org/"

sendMessage :: Conduit.Manager -> Types.APIToken -> Message.Message -> Word.Word64 -> IO (Conduit.Response LBS.ByteString)
sendMessage manager token message chatId = do
  let body =
        catMaybes
          [ Just ("chat_id", BS.packChars . show $ chatId)
          , getText' message
          , getReplyMarkup' message
          ]
  request <- Conduit.parseRequest $ apiURL ++ "bot" ++ token ++ "/sendMessage"
  let request' =
        Simple.setRequestMethod "POST" $
          Simple.setRequestBodyURLEncoded body request
  Conduit.httpLbs request' manager

getUpdates :: Conduit.Manager -> Types.APIToken -> UpdateParams.UpdateParams -> IO (Conduit.Response LBS.ByteString)
getUpdates manager token params = do
  let body = paramsToBody params
  request <- Conduit.parseRequest $ apiURL ++ "bot" ++ token ++ "/getUpdates"
  let request' =
        Simple.setRequestMethod "POST" $
          Simple.setRequestBodyURLEncoded body request
  Conduit.httpLbs request' manager
