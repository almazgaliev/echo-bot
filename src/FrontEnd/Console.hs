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

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot as EB
import qualified Logger (logInfo)
import qualified System.IO as SIO
import qualified Text.Read as TR (readMaybe)

newtype Handle = Handle {hBotHandle :: EB.Handle IO T.Text}

run :: Handle -> IO ()
run handle = do
  Logger.logInfo (EB.hLogHandle . hBotHandle $ handle) "Successfuly launched Console-Frontend instance"
  TIO.putStrLn "Welcome to the echo-bot!"
  prompt
  message <- TIO.getLine
  loop handle (EB.MessageEvent message)

loop :: Handle -> EB.Event T.Text -> IO ()
loop handle currentEvent = do
  responses <- EB.respond (hBotHandle handle) currentEvent
  printResponses responses
  prompt
  newMessage <- TIO.getLine
  newEvent <- case last responses of
    EB.MessageResponse _ -> pure (EB.MessageEvent newMessage)
    EB.MenuResponse _ buttons -> do
      case TR.readMaybe . T.unpack $ newMessage of
        Nothing -> printResponses [EB.MessageResponse "Error: please input number"] >> pure currentEvent
        Just count ->
          case lookup count buttons of
            Nothing -> printResponses [EB.MessageResponse "Error: please input valid number"] >> pure currentEvent
            Just _ -> pure (EB.SetRepetitionCountEvent count)
  loop handle newEvent

responseToText :: EB.Response T.Text -> T.Text
responseToText (EB.MessageResponse msg) = msg
responseToText (EB.MenuResponse title variants) = title <> "\n" <> T.intercalate ", " (T.pack . show . fst <$> variants)

printResponses :: [EB.Response T.Text] -> IO ()
printResponses = mapM_ ((\x -> TIO.putStr "Bot: " >> TIO.putStrLn x) . responseToText)

prompt :: IO ()
prompt = do
  TIO.putStr "You: "
  SIO.hFlush SIO.stdout
