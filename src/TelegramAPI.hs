{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module TelegramAPI (
  sendMessage,
  getUpdates,
  UpdateParams (..),
  APIToken,
  Message (..),
) where

import qualified Data.ByteString.Internal as BS (ByteString, packChars)
import qualified Data.ByteString.Lazy.Internal as LBS (ByteString)
import qualified Network.HTTP.Conduit as Conduit (Manager, Response, httpLbs, parseRequest)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Word as Word (Word64)
import qualified Network.HTTP.Simple as Simple (
  setRequestBodyURLEncoded,
  setRequestMethod,
 )
import Data.Maybe (catMaybes)

data UpdateParams = UpdateParams {getOffset :: Word.Word64, allowedUpdates :: [String]}

paramsToBody :: UpdateParams -> [(BS.ByteString, BS.ByteString)]
paramsToBody params = [("offset", BS.packChars . show $ getOffset params)]

type APIToken = String

newtype Message = Message {getText :: Maybe String}

getText' :: Message -> Maybe (BS.ByteString, BS.ByteString)
getText' message = ("text",) . T.encodeUtf8 . T.pack <$> getText message

apiURL :: String
apiURL = "https://api.telegram.org/"

sendMessage :: Conduit.Manager -> APIToken -> Message -> Word.Word64 -> IO (Conduit.Response LBS.ByteString)
sendMessage manager token message chatId = do
  let body = catMaybes
        [ Just ("chat_id", BS.packChars . show $ chatId)
        , getText' message
        ]
  request <- Conduit.parseRequest $ apiURL ++ "bot" ++ token ++ "/sendMessage"
  let request' =
        Simple.setRequestMethod "POST" $
          Simple.setRequestBodyURLEncoded body request
  Conduit.httpLbs request' manager

getUpdates :: Conduit.Manager -> APIToken -> UpdateParams -> IO (Conduit.Response LBS.ByteString)
getUpdates manager token params = do
  let body = paramsToBody params
  request <- Conduit.parseRequest $ apiURL ++ "bot" ++ token ++ "/getUpdates"
  let request' =
        Simple.setRequestMethod "POST" $
          Simple.setRequestBodyURLEncoded body request
  Conduit.httpLbs request' manager
