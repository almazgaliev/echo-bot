module TelegramAPI.Types.UpdateParams (UpdateParams (..)) where

import qualified Data.Word as Word

data UpdateParams = UpdateParams {getOffset :: Word.Word64, allowedUpdates :: [String]}
