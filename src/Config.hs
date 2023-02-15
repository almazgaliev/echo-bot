{-# LANGUAGE ImportQualifiedPost #-}

-- | A module to provide a configuration reader for other modules.
module Config (
  getBotConfig,
  getLoggerConfig,
  getFrontEndType,
)
where

import ConfigurationTypes qualified
import Data.Maybe (fromJust)
import Data.Text qualified as T
import Debug.Trace (traceShow)
import EchoBot qualified
import Logger (Level (Debug))
import Logger.Impl qualified
import System.IO (IOMode (WriteMode), openFile)
import Text.JSON

{- | Gets the bot config. In any case it can provide reasonable
 default values.
-}
getBotConfig :: IO EchoBot.Config
getBotConfig =
  return $
    EchoBot.Config
      { EchoBot.confHelpReply = T.pack "this is help message"
      , EchoBot.confRepeatReply = T.pack "The new repetition count is {count}."
      , EchoBot.confRepetitionCount = 2
      }

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  configJSObject <- readConfig
  let config = fromJSObject configJSObject -- FIX later handle errors
  let logLevel = read . fromJust . lookup "minLevel" $ config
  let path = fromJust . lookup "file" $ config
  handle <- openFile path WriteMode
  return $
    Logger.Impl.Config
      { Logger.Impl.confFileHandle = handle
      , Logger.Impl.confMinLevel = logLevel
      }

readConfig :: IO (JSObject String)
readConfig = do
  s <- readFile "logger.json"
  case Text.JSON.decodeStrict s of
    Text.JSON.Error msg -> error msg
    Ok obj -> return obj

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = return ConfigurationTypes.ConsoleFrontEnd
