module Main (main) where

import qualified Config
import qualified ConfigurationTypes
import qualified Data.IORef as DIOR
import qualified Data.Text as T
import qualified EchoBot as EB
import qualified FrontEnd.Console
import qualified FrontEnd.Telegram
import qualified Network.HTTP.Conduit as Conduit (Manager, newManager, tlsManagerSettings)

import qualified Data.Map as DM
import qualified Logger
import qualified Logger.Impl
import qualified System.Exit as Exit (die)
import qualified TelegramAPI.Types as Types

main :: IO ()
main = do
  withLogHandle $ \logHandle -> do
    frontEnd <- Config.getFrontEndType
    case frontEnd of
      ConfigurationTypes.TelegramFrontEnd -> do
        token <- readFile Config.tokenPath
        manager <- Conduit.newManager Conduit.tlsManagerSettings
        runTelegramFrontEnd logHandle manager token
      ConfigurationTypes.ConsoleFrontEnd -> do
        botHandle <- makeBotHandleForPlainText logHandle
        runConsoleFrontEnd botHandle

runConsoleFrontEnd :: EB.Handle IO T.Text -> IO ()
runConsoleFrontEnd botHandle =
  FrontEnd.Console.run
    FrontEnd.Console.Handle {FrontEnd.Console.hBotHandle = botHandle}

runTelegramFrontEnd :: Logger.Handle IO -> Conduit.Manager -> Types.APIToken -> IO ()
runTelegramFrontEnd logHandle manager token = do
  defaultHandle <- makeBotHandleForTelegram logHandle
  FrontEnd.Telegram.run
    defaultHandle
    FrontEnd.Telegram.Handle
      { FrontEnd.Telegram.hBotHandle = DM.empty
      , FrontEnd.Telegram.hManager = manager
      , FrontEnd.Telegram.hToken = token
      , FrontEnd.Telegram.hOffset = 0
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
makeBotHandleForPlainText :: Logger.Handle IO -> IO (EB.Handle IO T.Text)
makeBotHandleForPlainText logHandle = do
  botConfig <- Config.getBotConfig
  initialState <- either (Exit.die . T.unpack) pure $ EB.makeState botConfig
  stateRef <- DIOR.newIORef initialState
  return
    EB.Handle
      { EB.hGetState = DIOR.readIORef stateRef
      , EB.hModifyState' = DIOR.modifyIORef' stateRef
      , EB.hLogHandle = logHandle
      , EB.hConfig = botConfig
      , EB.hTextFromMessage = Just
      , EB.hMessageFromText = id
      }

makeBotHandleForTelegram :: Logger.Handle IO -> IO (EB.Handle IO a)
makeBotHandleForTelegram logHandle = do
  botConfig <- Config.getBotConfig
  initialState <- either (Exit.die . T.unpack) pure $ EB.makeState botConfig
  stateRef <- DIOR.newIORef initialState
  return
    EB.Handle
      { EB.hGetState = DIOR.readIORef stateRef
      , EB.hModifyState' = DIOR.modifyIORef' stateRef
      , EB.hLogHandle = logHandle
      , EB.hConfig = botConfig
      , EB.hTextFromMessage = error "Not Implemented"
      , EB.hMessageFromText = error "Not Implemented"
      }
