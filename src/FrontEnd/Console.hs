{-# LANGUAGE OverloadedStrings #-}

{- | The console front-end is responsible for console I/O and
 appropriate handling of other high-level bot interactions (menu
 output etc).
-}
module FrontEnd.Console (
  run,
  Handle (..),
)
where

import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import EchoBot qualified

newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }

run :: Handle -> IO ()
run handle = do
  TIO.putStrLn "Welcome to the echo-bot!"
  loop handle

-- 1. Read a line from the console.
-- 2. Send it to the bot, get its response and output it.
-- 3. Go to 1.
loop :: Handle -> IO ()
loop handle = do
  message <- TIO.getLine
  responses <- EchoBot.respond (hBotHandle handle) $ EchoBot.MessageEvent message
  mapM_ (TIO.putStrLn . responseToText) responses
  loop handle

responseToText :: EchoBot.Response T.Text -> T.Text
responseToText (EchoBot.MessageResponse msg) = msg
responseToText (EchoBot.MenuResponse _ _) = "Menu"
