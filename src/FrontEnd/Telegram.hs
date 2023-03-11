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
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Word as Word (Word64)
import qualified EchoBot as EB
import Logger ((.<))
import qualified Logger
import qualified Network.HTTP.Conduit as Conduit

import qualified TelegramAPI.Types as Types
import qualified TelegramAPI.Types.Message as Message
import qualified TelegramAPI.Types.UpdateParams as UpdateParams
import qualified TelegramAPI.Wrapper as TW
import qualified TelegramAPI.Wrapper.Types.ChatInfo as ChatInfo
import qualified TelegramAPI.Wrapper.Types.MessageInfo as MessageInfo
import qualified TelegramAPI.Wrapper.Types.SenderInfo as SenderInfo
import qualified TelegramAPI.Wrapper.Types.Updates as Updates
import qualified Util (wrapWithTicks)

data Handle m a = Handle
  { hBotHandle :: DM.Map Word.Word64 (EB.Handle m a)
  , hOffset :: Word.Word64
  , hManager :: Conduit.Manager
  , hToken :: Types.APIToken
  }

run :: EB.Handle IO a -> Handle IO a -> IO ()
run defaultHandle handle = do
  threadDelay $ secToMicro sec
  let logHandle = EB.hLogHandle defaultHandle
  let oldHandles = hBotHandle handle
  response <-
    TW.getUpdates
      (hManager handle)
      (hToken handle)
      ( UpdateParams.UpdateParams
          { UpdateParams.getOffset = hOffset handle
          , UpdateParams.allowedUpdates = ["message"]
          }
      )
  case response of
    Left msg -> do
      Logger.logDebug logHandle $ "getUpdates: " .< msg
      run defaultHandle handle
    Right (Updates.UpdatesInfo {Updates.getResult = updates}) -> do
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

respond :: Handle IO a -> (Word.Word64, String) -> IO (Either String ())
respond handle (chatId, msg) = do
  let manager = hManager handle
  let token = hToken handle
  let message = Message.Message {Message.getText = Just msg, Message.getMarkup = Nothing}
  TW.sendMessage manager token chatId message

getChatId :: Updates.UpdateInfo -> Maybe Word.Word64
getChatId = return . ChatInfo.getChatId . MessageInfo.getChatInfo <=< Updates.getMessage

getTextOfMsg :: Updates.UpdateInfo -> Maybe String
getTextOfMsg = return . T.unpack <=< MessageInfo.getText <=< Updates.getMessage

showUpdateData :: Updates.UpdateInfo -> String
showUpdateData u =
  let
    msg = Updates.getMessage u
   in
    concat
      [ maybe "[No Name]" (SenderInfo.getFirstName . MessageInfo.getSender) msg
      , "("
      , maybe "[No UserName]" (SenderInfo.getUserName . MessageInfo.getSender) msg
      , "):"
      , maybe "[No Text]" Util.wrapWithTicks (getTextOfMsg u)
      ]

getMaxUpdateId :: [Updates.UpdateInfo] -> Word.Word64
getMaxUpdateId = List.foldl' max 0 . fmap Updates.getUpdateId

sec :: Int
sec = 3

secToMicro :: Num a => a -> a
secToMicro = (* 1000000)