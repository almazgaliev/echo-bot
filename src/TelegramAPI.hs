{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI (
  sendMessage,
  getUpdates,
) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Internal (ByteString)
import Network.HTTP.Client (Manager, Response, httpLbs, newManager, parseRequest)

import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Simple (
  getResponseBody,
  getResponseHeader,
  getResponseStatusCode,
  parseRequest,
  setRequestBodyURLEncoded,
  setRequestMethod,
 )

apiURL = "https://api.telegram.org/"

-- manager <- newManager tlsManagerSettings
sendMessage :: Manager -> String -> String -> Int -> IO (Response ByteString)
sendMessage manager token message chatId = do
  let echoBody =
        [ ("chat_id", pack . show $ chatId)
        , ("text", pack message)
        ]
  request <- parseRequest $ apiURL ++ "bot" ++ token ++ "/sendMessage"
  let request' =
        setRequestMethod "POST" $
          setRequestBodyURLEncoded echoBody request
  httpLbs request' manager

getUpdates :: Manager -> String -> IO (Response ByteString)
getUpdates manager token = do
  request <- parseRequest $ apiURL ++ "bot" ++ token ++ "/getUpdates"
  let request' = setRequestMethod "POST" request
  httpLbs request' manager