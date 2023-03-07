{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module ConfigurationTypes (
  FrontEndType (..),
)
where

import qualified Data.Aeson as Aeson (FromJSON)
import qualified GHC.Generics as Generics (Generic)

data FrontEndType
  = ConsoleFrontEnd
  | TelegramFrontEnd
  deriving (Generics.Generic, Aeson.FromJSON)