module Telegram.Bot.API.Types.UpdateParams (UpdateParams (..)) where

import qualified Data.Text as T
import qualified Data.Word as Word

data UpdateParams = UpdateParams {getOffset :: Word.Word64, allowedUpdates :: [T.Text]}
