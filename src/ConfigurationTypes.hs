{-# LANGUAGE DeriveGeneric #-}

module ConfigurationTypes (
  FrontEndType (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data FrontEndType
  = ConsoleFrontEnd
  | TelegramFrontEnd
  deriving (Generic)

instance FromJSON FrontEndType
instance ToJSON FrontEndType
