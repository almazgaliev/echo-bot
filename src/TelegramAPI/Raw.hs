{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI.Raw (
  sendMessage,
  getUpdates,
) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy.Internal (ByteString)
import Network.HTTP.Client (Manager, Response, httpLbs, parseRequest)

import Network.HTTP.Simple (
  setRequestBodyURLEncoded,
  setRequestMethod,
 )

apiURL :: String
apiURL = "https://api.telegram.org/"

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
