module TelegramAPI.Types.Message (Message(..)) where
import qualified TelegramAPI.Types.Markup as Markup

data Message = Message {getText :: Maybe String, getMarkup :: Maybe Markup.Markup}
