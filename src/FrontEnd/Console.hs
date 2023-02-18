{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{- | The console front-end is responsible for console I/O and
 appropriate handling of other high-level bot interactions (menu
 output etc).
-}
module FrontEnd.Console (
  run,
  Handle' (..),
)
where

import Data.Maybe (fromMaybe)
import Data.Text (intercalate)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import EchoBot qualified as EB
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Text.Read (readMaybe)

newtype Handle' = Handle
  { hBotHandle :: EB.Handle IO T.Text
  }

run :: Handle' -> IO ()
run handle = do
  TIO.putStrLn "Welcome to the echo-bot!"
  prompt
  message <- TIO.getLine
  loop handle (EB.MessageEvent message)

loop :: Handle' -> EB.Event T.Text -> IO ()
loop handle event = do
  responses <- EB.respond (hBotHandle handle) event
  printResponses responses
  prompt
  msg <- TIO.getLine
  let newEvent = case last responses of
        EB.MessageResponse _ -> EB.MessageEvent msg
        EB.MenuResponse _ buttons ->
          fromMaybe
            (EB.MessageEvent "/help")
            ((readMaybe . T.unpack $ msg) >>= (`lookup` buttons))
  loop handle newEvent

responseToText :: EB.Response T.Text -> T.Text
responseToText (EB.MessageResponse msg) = msg
responseToText (EB.MenuResponse title variants) = title <> "\n" <> intercalate ", " (T.pack . show . fst <$> variants)

printResponses :: [EB.Response T.Text] -> IO ()
printResponses = mapM_ ((\x -> TIO.putStr "Bot: " >> TIO.putStrLn x) . responseToText)

prompt :: IO ()
prompt = do
  TIO.putStr "You: "
  hFlush stdout
