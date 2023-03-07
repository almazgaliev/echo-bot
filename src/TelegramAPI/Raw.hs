{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI (
  sendMessage,
  getUpdates,
  UpdateParams (..),
  APIToken,
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

data UpdateParams = UpdateParams {getOffset :: Word.Word64, allowedUpdates :: [String]}

paramsToBody :: UpdateParams -> [(BS.ByteString, BS.ByteString)]
paramsToBody params = [("offset", BS.packChars . show $ getOffset params)]

type APIToken = String
apiURL :: String
apiURL = "https://api.telegram.org/"

sendMessage :: Conduit.Manager -> APIToken -> String -> Word.Word64 -> IO (Conduit.Response LBS.ByteString)
sendMessage manager token message chatId = do
  let body =
        [ ("chat_id", BS.packChars . show $ chatId)
        , ("text", T.encodeUtf8 . T.pack $ message)
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
