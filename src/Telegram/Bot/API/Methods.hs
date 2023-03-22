{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Telegram.Bot.API.Methods (
  sendMessage,
  getUpdates,
  answerCallbackQuery,
  sendPhoto,
) where

import qualified Data.ByteString.Internal as BS (packChars)
import qualified Network.HTTP.Conduit as Conduit (Manager, Response, httpLbs, parseRequest)

-- import qualified Data.Aeson.KeyMap as AKM
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Client as Network.HTTP.Client.Types

import qualified Data.Aeson as Aeson

-- import qualified Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Network.HTTP.Simple as Simple (
  setRequestBodyURLEncoded,
  setRequestMethod,
 )
import qualified Telegram.Bot.API.Types as Types
import qualified Telegram.Bot.API.Types.AnswerCallbackQueryParams as AnswerCallbackQueryParams
import qualified Telegram.Bot.API.Types.SendPhotoParams as Photo
import qualified Telegram.Bot.API.Types.UpdateParams as UpdateParams
import qualified Telegram.Bot.API.Wrapper.Types.Chat as Chat
import qualified Telegram.Bot.API.Wrapper.Types.Message as Message

type RequestBody = [(BS.ByteString, BS.ByteString)]

-- pairsList :: AKM.KeyMap Aeson.Value -> [(BS.ByteString, BS.ByteString)]
-- pairsList v = Data.Bifunctor.bimap f f' <$> filter (\pair -> snd pair /= Aeson.Null) (AKM.toList v)
--  where
--   f = LBS.toStrict . Aeson.encode
--   f' = LBS.toStrict . Aeson.encode

updateParamsToBody :: UpdateParams.UpdateParams -> RequestBody
updateParamsToBody params = [("offset",) . BS.packChars . show $ UpdateParams.getOffset params]

answerParamsToBody :: AnswerCallbackQueryParams.AnswerCallbackQueryParams -> RequestBody
answerParamsToBody params =
  catMaybes
    [ pure . ("callback_query_id",) . Encoding.encodeUtf8 $ AnswerCallbackQueryParams.getCallbackQueryId params
    , ("text",) . Encoding.encodeUtf8 <$> AnswerCallbackQueryParams.getText params -- BS.packChars
    ]

messageToBody :: Message.Message -> RequestBody
messageToBody message =
  catMaybes
    [ pure ("chat_id", BS.packChars . show . Chat.getId . Message.getChat $ message)
    , ("text",) . Encoding.encodeUtf8 <$> Message.getText message
    , ("entities",) . LBS.toStrict . Aeson.encode <$> Message.getEntities message
    , ("reply_markup",) . LBS.toStrict . Aeson.encode <$> Message.getMarkup message
    ]

photoToBody :: Photo.SendPhotoParams -> RequestBody
photoToBody photo =
  catMaybes
    [ pure ("chat_id", BS.packChars . show . Photo.getChatId $ photo)
    , pure ("photo", Encoding.encodeUtf8 . T.pack $ Photo.getPhoto photo)
    , ("caption",) . LBS.toStrict . Aeson.encode <$> Photo.getCaption photo
    , ("caption_entities",) . LBS.toStrict . Aeson.encode <$> Photo.getCaptionEntities photo
    ]

apiURL :: String
apiURL = "https://api.telegram.org/"

apiPOSTRequest :: String -> String -> RequestBody -> IO Network.HTTP.Client.Types.Request
apiPOSTRequest token method body = do
  request <- Conduit.parseRequest $ apiURL ++ "bot" ++ token ++ method
  return $
    Simple.setRequestMethod "POST" $
      Simple.setRequestBodyURLEncoded body request

sendMessage :: Conduit.Manager -> Types.APIToken -> Message.Message -> IO (Conduit.Response LBS.ByteString)
sendMessage manager token message = do
  let body = messageToBody message
  request' <- apiPOSTRequest token "/sendMessage" body
  Conduit.httpLbs request' manager

sendPhoto :: Conduit.Manager -> Types.APIToken -> Photo.SendPhotoParams -> IO (Conduit.Response LBS.ByteString)
sendPhoto manager token photo = do
  let body = photoToBody photo
  request' <- apiPOSTRequest token "/sendPhoto" body
  Conduit.httpLbs request' manager

getUpdates :: Conduit.Manager -> Types.APIToken -> UpdateParams.UpdateParams -> IO (Conduit.Response LBS.ByteString)
getUpdates manager token params = do
  let body = updateParamsToBody params
  request' <- apiPOSTRequest token "/getUpdates" body
  Conduit.httpLbs request' manager

answerCallbackQuery :: Conduit.Manager -> Types.APIToken -> AnswerCallbackQueryParams.AnswerCallbackQueryParams -> IO (Conduit.Response LBS.ByteString)
answerCallbackQuery manager token params = do
  let body = answerParamsToBody params
  request' <- apiPOSTRequest token "/answerCallbackQuery" body
  Conduit.httpLbs request' manager
