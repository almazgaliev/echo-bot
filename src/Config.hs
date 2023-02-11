{-# LANGUAGE ImportQualifiedPost #-}

-- | A module to provide a configuration reader for other modules.
module Config (
  getBotConfig,
  getLoggerConfig,
  getFrontEndType,
)
where

import ConfigurationTypes qualified
import Data.Text qualified as T
import EchoBot qualified
import GHC.IO (unsafePerformIO)
import GHC.IO.IOMode (IOMode (WriteMode))
import Logger (Level (..))
import Logger.Impl qualified
import System.IO (openFile)

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
getLoggerConfig =
  return $
    Logger.Impl.Config
      { Logger.Impl.confFileHandle = unsafePerformIO $ openFile "./log.txt" WriteMode -- FIX
      -- Logger.Impl.confFileHandle = FileHandle "log.txt" $ GHC.MVar.MVar ,
      , Logger.Impl.confMinLevel = Debug
      }

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = return ConfigurationTypes.ConsoleFrontEnd
