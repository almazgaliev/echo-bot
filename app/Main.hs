{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Config
import qualified ConfigurationTypes
import qualified Data.IORef as DIOR
import qualified Data.Text as T
import qualified EchoBot
import qualified FrontEnd.Console
import qualified FrontEnd.Telegram
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.Map as DM
import Logger (logInfo, (.<))
import qualified Logger
import qualified Logger.Impl
import System.Exit (die)
import qualified TelegramAPI.Wrappers as TW

main :: IO ()
main = do
  withLogHandle $ \logHandle -> do
    frontEnd <- Config.getFrontEndType
    case frontEnd of
      ConfigurationTypes.TelegramFrontEnd -> do
        token <- readFile Config.tokenPath
        manager <- newManager tlsManagerSettings
        runTelegramFrontEnd logHandle manager token
      ConfigurationTypes.ConsoleFrontEnd -> do
        botHandle <- makeBotHandleForPlainText logHandle
        runConsoleFrontEnd botHandle

runConsoleFrontEnd :: EchoBot.Handle IO T.Text -> IO ()
runConsoleFrontEnd botHandle =
  FrontEnd.Console.run
    FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}

runTelegramFrontEnd :: Logger.Handle IO -> Manager -> String -> IO ()
runTelegramFrontEnd logHandle manager token = do
  res <- TW.getUpdates manager token
  case res of
    Left s -> logInfo logHandle $ "getUpdates:" .< s
    Right ui -> do
      handle <- makeBotHandleForTelegram logHandle
      let m = DM.fromList $ zip (TW.id . TW.chatInfo . TW.message <$> TW.result ui) (repeat handle)
      FrontEnd.Telegram.run
        FrontEnd.Telegram.Handle {
          FrontEnd.Telegram.hBotHandle = m,
          FrontEnd.Telegram.hManager = manager,
          FrontEnd.Telegram.hToken = token
        }

withLogHandle :: (Logger.Handle IO -> IO ()) -> IO ()
withLogHandle f = do
  config <- Config.getLoggerConfig
  Logger.Impl.withHandle config f

{- | Creates a bot handle. Please note:

 * a handle holds a reference to an 'IORef' with a 'EchoBot.State',
   so that it can only keep state of a single user. In order to
   support multiple users in a chat, you should create a new handle
   for each user and probably keep them in a 'Data.Map' keyed by a
   user id.

 * 'EchoBot.Handle' is parameterized with the 'Text' type, so that
   it supports only plain text messages suitable for the console.
   When implementing Telegram or another multimedia chat support,
   you should create a similar function, but parameterized with
   another message type which can represent either text or
   multimedia messages. You will need to specify different functions
   @hMessageFromText@ and @hTextFromMessage@.
-}
makeBotHandleForPlainText :: Logger.Handle IO -> IO (EchoBot.Handle IO T.Text)
makeBotHandleForPlainText logHandle = do
  botConfig <- Config.getBotConfig
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  stateRef <- DIOR.newIORef initialState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = DIOR.readIORef stateRef
      , EchoBot.hModifyState' = DIOR.modifyIORef' stateRef
      , EchoBot.hLogHandle = logHandle
      , EchoBot.hConfig = botConfig
      , EchoBot.hTextFromMessage = Just
      , EchoBot.hMessageFromText = id
      }

makeBotHandleForTelegram :: Logger.Handle IO -> IO (EchoBot.Handle IO a)
makeBotHandleForTelegram logHandle = do
  botConfig <- Config.getBotConfig
  initialState <- either (die . T.unpack) pure $ EchoBot.makeState botConfig
  stateRef <- DIOR.newIORef initialState
  pure
    EchoBot.Handle
      { EchoBot.hGetState = DIOR.readIORef stateRef
      , EchoBot.hModifyState' = DIOR.modifyIORef' stateRef
      , EchoBot.hLogHandle = logHandle
      , EchoBot.hConfig = botConfig
      , EchoBot.hTextFromMessage = error "Not Implemented"
      , EchoBot.hMessageFromText = error "Not Implemented"
      }