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

import qualified Config
import Control.Monad ((<=<))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IORef as DIOR
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T (unpack)
import qualified Data.Word as Word (Word64)
import qualified EchoBot as EB
import Logger ((.<))
import qualified Logger
import qualified Network.HTTP.Conduit as Conduit
import qualified System.Exit as Exit (die)
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

makeBotHandleForTelegram :: Logger.Handle IO -> IO (EB.Handle IO a)
makeBotHandleForTelegram logHandle = do
  botConfig <- Config.getBotConfig
  initialState <- either (Exit.die . T.unpack) pure $ EB.makeState botConfig
  stateRef <- DIOR.newIORef initialState
  return
    EB.Handle
      { EB.hGetState = DIOR.readIORef stateRef
      , EB.hModifyState' = DIOR.modifyIORef' stateRef
      , EB.hLogHandle = logHandle
      , EB.hConfig = botConfig
      , EB.hTextFromMessage = error "Not Implemented"
      , EB.hMessageFromText = error "Not Implemented"
      }

data Handle m a = Handle
  { hBotHandle :: DM.Map Word.Word64 (EB.Handle m a)
  , hOffset :: Word.Word64
  , hManager :: Conduit.Manager
  , hToken :: TelegramAPI.APIToken
  }

run :: Handle IO a -> Logger.Handle IO -> IO ()
run handle logHandle = do
  threadDelay $ sec * 1000000
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
      -- run (handle {hOffset = hOffset handle + 1}) logHandle
      run handle logHandle
    Right (TW.UpdatesInfo {TW.getResult = updates}) -> do
      Logger.logDebug logHandle $ "got these updates: " .< ("\n" ++ Prelude.unlines (fmap showUpdateData updates))
      h <- makeBotHandleForTelegram logHandle
      let ids = mapMaybe getChatId updates
      let messages = mapMaybe getTextOfMsg updates
      let newHandles = DM.fromList $ zip ids (repeat h)
      let a = DM.union oldHandles newHandles
      let newHandle = handle {hBotHandle = a, hOffset = getMaxUpdateId updates + 1}
      forM_ (zip ids messages) $
        respond newHandle
      run newHandle logHandle

respond :: Handle IO a -> (Word.Word64, String) -> IO (Conduit.Response LBS.ByteString)
respond handle (chatId, msg) = do
  let manager = hManager handle
  let token = hToken handle
  TelegramAPI.sendMessage manager token msg chatId

getChatId :: TW.UpdateInfo -> Maybe Word.Word64
getChatId upd = TW.getChatId . TW.getChatInfo <$> TW.getMessage upd

getTextOfMsg :: TW.UpdateInfo -> Maybe String
getTextOfMsg = return . T.unpack <=< TW.getText <=< TW.getMessage

showUpdateData :: TW.UpdateInfo -> String
showUpdateData u =
  maybe "---" (TW.getFirstName . TW.getSender) (TW.getMessage u)
    ++ "("
    ++ maybe "---" (TW.getUserName . TW.getSender) (TW.getMessage u)
    ++ ")"
    ++ ":"
    ++ fromMaybe "No Text" (getTextOfMsg u)

getMaxUpdateId :: [TW.UpdateInfo] -> Word.Word64
getMaxUpdateId [] = 0
getMaxUpdateId updates = Prelude.maximum . fmap TW.getUpdateId $ updates

sec :: Int
sec = 3