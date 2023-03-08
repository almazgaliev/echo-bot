{-# LANGUAGE OverloadedStrings #-}

{- | The console front-end is responsible for console I/O and
 appropriate handling of other high-level bot interactions (menu
 output etc).
-}
module FrontEnd.Telegram (
  run,
  Handle (..),
)
where

import Control.Concurrent (threadDelay)
import Data.Foldable (forM_)

import qualified Data.Map as DM

import Control.Monad (unless, (<=<))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (mapMaybe)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Word as Word (Word64)
import qualified EchoBot as EB
import Logger ((.<))
import qualified Logger
import qualified Network.HTTP.Conduit as Conduit
import qualified TelegramAPI (APIToken, UpdateParams (UpdateParams, allowedUpdates, getOffset), sendMessage)
import qualified TelegramAPI.Wrappers as TW (
  ChatInfo (getChatId),
  MessageInfo (getChatInfo, getSender, getText),
  SenderInfo (getFirstName, getUserName),
  UpdateInfo (getMessage),
  UpdatesInfo (UpdatesInfo, getResult),
  getUpdateId,
  getUpdates,
 )
import qualified Util (wrapWithTicks)
import qualified Data.List as List

data Handle m a = Handle
  { hBotHandle :: DM.Map Word.Word64 (EB.Handle m a)
  , hOffset :: Word.Word64
  , hManager :: Conduit.Manager
  , hToken :: TelegramAPI.APIToken
  }

run :: EB.Handle IO a -> Handle IO a -> IO ()
run defaultHandle handle = do
  threadDelay $ sec * 1000000
  let logHandle = EB.hLogHandle defaultHandle
  let oldHandles = hBotHandle handle
  response <-
    TW.getUpdates
      (hManager handle)
      (hToken handle)
      ( TelegramAPI.UpdateParams
          { TelegramAPI.getOffset = hOffset handle
          , TelegramAPI.allowedUpdates = ["message"]
          }
      )
  case response of
    Left msg -> do
      Logger.logDebug logHandle $ "getUpdates: " .< msg
      run defaultHandle handle
    Right (TW.UpdatesInfo {TW.getResult = updates}) -> do
      let updateLog = List.intercalate ", " (fmap showUpdateData updates)
      unless (null updateLog) $ Logger.logDebug logHandle . T.pack $ "got these updates: " ++ updateLog
      let ids = mapMaybe getChatId updates
      let messages = mapMaybe getTextOfMsg updates
      let newHandles = DM.fromList $ zip ids (repeat defaultHandle)
      let a = DM.union oldHandles newHandles
      let newHandle = handle {hBotHandle = a, hOffset = getMaxUpdateId updates + 1}
      forM_ (zip ids messages) $
        respond newHandle
      run defaultHandle newHandle

respond :: Handle IO a -> (Word.Word64, String) -> IO (Conduit.Response LBS.ByteString)
respond handle (chatId, msg) = do
  let manager = hManager handle
  let token = hToken handle
  TelegramAPI.sendMessage manager token msg chatId

getChatId :: TW.UpdateInfo -> Maybe Word.Word64
getChatId = return . TW.getChatId . TW.getChatInfo <=< TW.getMessage

getTextOfMsg :: TW.UpdateInfo -> Maybe String
getTextOfMsg = return . T.unpack <=< TW.getText <=< TW.getMessage

showUpdateData :: TW.UpdateInfo -> String
showUpdateData u =
  maybe "---" (TW.getFirstName . TW.getSender) (TW.getMessage u)
    ++ "("
    ++ maybe "---" (TW.getUserName . TW.getSender) (TW.getMessage u)
    ++ ")"
    ++ ":"
    ++ maybe "[No Text]" Util.wrapWithTicks (getTextOfMsg u)

getMaxUpdateId :: [TW.UpdateInfo] -> Word.Word64
getMaxUpdateId = List.foldl' max 0 . fmap TW.getUpdateId

sec :: Int
sec = 3