module Telegram.Bot.API.Types.Message (Message (..)) where

import qualified Telegram.Bot.API.Types.Markup as Markup

data Message = Message {getText :: Maybe String, getMarkup :: Maybe Markup.Markup}
