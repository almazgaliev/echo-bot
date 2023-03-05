{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

-- | A module to provide a configuration reader for other modules.
module Config (
  getBotConfig,
  getLoggerConfig,
  getFrontEndType,
  tokenPath
)
where

import ConfigurationTypes qualified
import Data.Text qualified as T
import EchoBot qualified
import Logger qualified
import Logger.Impl qualified
import System.FilePath (isValid)
import System.IO (IOMode (..), openFile, stderr, stdout)

import Data.Aeson (FromJSON , ToJSON, eitherDecode', withText)
import Data.Aeson.Types qualified as AT
import Data.ByteString.Lazy.Char8 qualified as BSL
import Data.Text (unpack)
import GHC.Generics (Generic)

data BotConfig = BotConfig {frontend :: ConfigurationTypes.FrontEndType, conf :: EchoBot.Config} deriving (Generic)

instance FromJSON BotConfig
instance ToJSON BotConfig

data ValidHandle = FileHandle FilePath | Stdout | Stderr

instance FromJSON ValidHandle where
  parseJSON =
    let
      f "stdout" = return Stdout
      f "stderr" = return Stderr
      f path | isValid path = return $ FileHandle path
      f hnd = fail $ "handle: \"" ++ hnd ++ "\" is invalid. valid values are stderr, stdout or any valid file path"
     in
      withText "Handle" (f . unpack)
instance ToJSON ValidHandle where
  toJSON (FileHandle f) = AT.String . T.pack $ f
  toJSON Stdout = AT.String "stdout"
  toJSON Stderr = AT.String "stderr"

data ValidLoggerConfig = LoggerConfig {handle :: ValidHandle, minLevel :: Logger.Level} deriving (Generic, FromJSON)




{- | Gets the bot config. In any case it can provide reasonable
 default values.
-}
getBotConfig :: IO EchoBot.Config
getBotConfig = do
  json <- BSL.readFile "Config/botConfig.json"
  let res = eitherDecode' json :: Either String BotConfig
  case res of
    Right config -> do
      return . conf $ config
    Left msg -> fail msg

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  json <- BSL.readFile "Config/logger.json"
  let res = eitherDecode' json
  case res of
    Right config -> do
      h <- case handle config of
        FileHandle s -> openFile s AppendMode
        Stdout -> pure stdout
        Stderr -> pure stderr
      return
        Logger.Impl.Config
          { Logger.Impl.confHandle = h
          , Logger.Impl.confMinLevel = minLevel config
          }
    Left msg -> fail msg

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = do
  json <- BSL.readFile "Config/botConfig.json"
  let res = eitherDecode' json
  case res of
    Right config -> do
      return $ frontend config
    Left msg -> fail msg

tokenPath :: String
tokenPath = "Config/token.txt"