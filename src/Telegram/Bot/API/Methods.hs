{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Telegram.Bot.API.Methods (
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
import qualified Network.HTTP.Client as Network.HTTP.Client.Types
import qualified Network.HTTP.Simple as Simple (
  setRequestBodyURLEncoded,
  setRequestMethod,
 )
import qualified Telegram.Bot.API.Types as Types
import qualified Telegram.Bot.API.Types.AnswerCallbackQueryParams as AnswerCallbackQueryParams
import qualified Telegram.Bot.API.Types.Message as Message
import qualified Telegram.Bot.API.Types.UpdateParams as UpdateParams

type RequestBody = [(BS.ByteString, BS.ByteString)]

updateParamsToBody :: UpdateParams.UpdateParams -> RequestBody
updateParamsToBody params = [("offset", BS.packChars . show $ UpdateParams.getOffset params)]

answerParamsToBody :: AnswerCallbackQueryParams.AnswerCallbackQueryParams -> RequestBody
answerParamsToBody params =
  catMaybes
    [ pure . ("callback_query_id",) . BS.packChars $ AnswerCallbackQueryParams.getCallbackQueryId params
    , ("text",) . BS.packChars <$> AnswerCallbackQueryParams.getText params
    ]

getReplyMarkup' :: Message.Message -> Maybe (BS.ByteString, BS.ByteString)
getReplyMarkup' message = ("reply_markup",) . LBS.toStrict . Aeson.encode <$> Message.getMarkup message

getText' :: Message.Message -> Maybe (BS.ByteString, BS.ByteString)
getText' message = ("text",) . T.encodeUtf8 . T.pack <$> Message.getText message

apiURL :: String
apiURL = "https://api.telegram.org/"

apiPOSTRequest :: String -> String -> RequestBody -> IO Network.HTTP.Client.Types.Request
apiPOSTRequest token method body = do
  request <- Conduit.parseRequest $ apiURL ++ "bot" ++ token ++ method
  return $
    Simple.setRequestMethod "POST" $
      Simple.setRequestBodyURLEncoded body request

sendMessage :: Conduit.Manager -> Types.APIToken -> Word.Word64 -> Message.Message -> IO (Conduit.Response LBS.ByteString)
sendMessage manager token chatId message = do
  let body =
        catMaybes
          [ pure ("chat_id", BS.packChars . show $ chatId)
          , getText' message
          , getReplyMarkup' message
          ]
  request' <- apiPOSTRequest token "/sendMessage" body
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
