{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module Config (
  getBotConfig,
  getLoggerConfig,
  getFrontEndType,
  tokenPath,
)
where

import qualified ConfigurationTypes (FrontEndType)
import qualified EchoBot as EB
import qualified Logger
import qualified Logger.Impl
import qualified System.FilePath as FP (isValid)
import qualified System.IO as SIO (IOMode (..), openFile, stderr, stdout)

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson (FromJSON (parseJSON), eitherDecode', withObject, withText)
import qualified Data.ByteString.Lazy.Char8 as LBS (readFile)
import qualified Data.Text as T (unpack)
import qualified GHC.Generics as Generics (Generic)

newtype BotConfig = BotConfig {conf :: EB.Config} deriving (Generics.Generic)

instance Aeson.FromJSON BotConfig where
  parseJSON = Aeson.withObject "BotConfig" $ \v -> BotConfig <$> (EB.Config <$> v .: "confHelpReply" <*> v .: "confRepeatReply" <*> v .: "confRepetitionCount")

-- проверить если confRepetitionCount в пределах от 1 до 5

data ValidHandle = FileHandle FilePath | Stdout | Stderr

instance Aeson.FromJSON ValidHandle where
  parseJSON =
    let
      f "stdout" = return Stdout
      f "stderr" = return Stderr
      f path | FP.isValid path = return $ FileHandle path
      f hnd = fail $ "handle: \"" ++ hnd ++ "\" is invalid. valid values are stderr, stdout or any valid file path"
     in
      Aeson.withText "Handle" (f . T.unpack)

-- withObject "Handle" $ \v -> case AKM.lookup "frontend" v of
--   Just (String txt) -> (f . T.unpack) txt
--   _ -> fail "'frontend' key was not found"

data ValidLoggerConfig = LoggerConfig {handle :: ValidHandle, minLevel :: Logger.Level} deriving (Generics.Generic, Aeson.FromJSON)

{- | Gets the bot config. In any case it can provide reasonable
 default values.
-}
newtype FrontendConfig = FrontendConfig {getFrontend :: ConfigurationTypes.FrontEndType}

instance Aeson.FromJSON FrontendConfig where
  parseJSON = Aeson.withObject "FrontendConfig" $ \v -> FrontendConfig <$> v .: "frontend"

getBotConfig :: IO EB.Config
getBotConfig = do
  json <- LBS.readFile "Config/botConfig.json"
  let res = Aeson.eitherDecode' json :: Either String BotConfig
  case res of
    Right config -> do
      return . conf $ config
    Left msg -> fail msg

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig = do
  json <- LBS.readFile "Config/logger.json"
  let res = Aeson.eitherDecode' json
  case res of
    Right config -> do
      h <- case handle config of
        FileHandle s -> SIO.openFile s SIO.AppendMode
        Stdout -> pure SIO.stdout
        Stderr -> pure SIO.stderr
      return
        Logger.Impl.Config
          { Logger.Impl.confHandle = h
          , Logger.Impl.confMinLevel = minLevel config
          }
    Left msg -> fail msg

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = do
  json <- LBS.readFile "Config/frontendConfig.json"
  let res = Aeson.eitherDecode' json
  case res of
    Right config -> do
      return . getFrontend $ config
    Left msg -> fail msg

tokenPath :: String
tokenPath = "Config/token.txt"