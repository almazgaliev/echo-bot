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
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Word as Word (Word64)
import qualified EchoBot as EB
import Logger ((.<))
import qualified Logger
import qualified Network.HTTP.Conduit as Conduit

import qualified Telegram.Bot.API.Types as Types
import qualified Telegram.Bot.API.Types.AnswerCallbackQueryParams as AnswerCallbackQueryParams
import qualified Telegram.Bot.API.Types.UpdateParams as UpdateParams
import qualified Telegram.Bot.API.Wrapper.Methods as Methods
import qualified Telegram.Bot.API.Wrapper.Types.CallBackQuery as CallBackQuery
import qualified Telegram.Bot.API.Wrapper.Types.Chat as Chat
import qualified Telegram.Bot.API.Wrapper.Types.InlineKeyboardButton as InlineKeyboardButton
import qualified Telegram.Bot.API.Wrapper.Types.Markup as Markup
import qualified Telegram.Bot.API.Wrapper.Types.Message as Message
import qualified Telegram.Bot.API.Wrapper.Types.Updates as Updates
import qualified Telegram.Bot.API.Wrapper.Types.User as User
import qualified Text.Read as TR

data Handle m a = Handle
  { hBotHandle :: DM.Map Word.Word64 (EB.Handle m a)
  , hOffset :: Word.Word64
  , hManager :: Conduit.Manager
  , hToken :: Types.APIToken
  }

run :: EB.Handle IO Message.Message -> Handle IO Message.Message -> IO ()
run defaultHandle handleTele = do
  threadDelay $ secToMicro sec
  let logHandle = EB.hLogHandle defaultHandle
  updates <-
    Methods.getUpdates
      (hManager handleTele)
      (hToken handleTele)
      ( UpdateParams.UpdateParams
          { UpdateParams.getOffset = hOffset handleTele
          , UpdateParams.allowedUpdates = ["message"]
          }
      )
  case updates of
    Left msg -> do
      Logger.logDebug logHandle $ "getUpdates: " .< msg
      run defaultHandle handleTele
    Right (Updates.Updates {Updates.getResult = upds}) -> do
      newHandleTele <- processUpdates defaultHandle handleTele upds
      run defaultHandle newHandleTele

processUpdates :: EB.Handle IO Message.Message -> Handle IO Message.Message -> [Updates.Update] -> IO (Handle IO Message.Message)
processUpdates defaultHandle handleTele updates = do
  let currentHandles = hBotHandle handleTele
  let ids = mapMaybe getChatId updates
  let newHandles = DM.fromList $ zip ids (repeat defaultHandle)
  forM_ (mapMaybe getIdAndMessage updates) $ \(chatId, event) -> do
    responses <- EB.respond (fromMaybe defaultHandle (DM.lookup chatId currentHandles)) event
    let responses' = fmap (uncurry responseToMessage) (zip responses $ repeat chatId)
    let manager = hManager handleTele
    let token = hToken handleTele
    forM_ responses' $ Methods.sendMessage manager token

  forM_
    (mapMaybe getIdAndCallback updates)
    $ \(chatId, callBackId, event) -> do
      responses <- EB.respond (fromMaybe defaultHandle (DM.lookup chatId currentHandles)) event
      let responses' = fmap (uncurry responseToMessage) (zip responses $ repeat chatId)
      let manager = hManager handleTele
      let token = hToken handleTele
      forM_ responses' $ \message -> do
        _ <- Methods.sendMessage manager token message
        let params =
              AnswerCallbackQueryParams.AnswerCallbackQueryParams
                { AnswerCallbackQueryParams.getText = pure (T.append "Repetition count is set to: " . fromMaybe "" . Message.getText $ message)
                , AnswerCallbackQueryParams.getCallbackQueryId = callBackId
                }
        Methods.answerCallbackQuery manager token params
  return
    handleTele
      { hBotHandle = DM.union currentHandles newHandles
      , hOffset = getMaxUpdateId updates + 1
      }

responseToMessage :: EB.Response Message.Message -> Word.Word64 -> Message.Message
responseToMessage response chatId = case response of
  EB.MessageResponse msg -> msg {Message.getChat = Chat.Chat chatId}
  EB.MenuResponse txt buttons ->
    Message.Message
      { Message.getText = pure txt
      , Message.getMarkup = pure $ mkInlineKeyboard buttons
      , Message.getChat = Chat.Chat chatId
      , Message.getSender = Nothing
      }

mkInlineKeyboard :: [(Int, EB.Event a)] -> Markup.Markup
mkInlineKeyboard lst = Markup.InlineKeyboard [(\(x, _) -> let x' = T.pack $ show x in InlineKeyboardButton.mkInlineCallBackButton x' x') <$> lst]

-- a = const (EB.State (read msg))

getChatId :: Updates.Update -> Maybe Word.Word64
getChatId = return . Chat.getChatId . Message.getChat <=< Updates.getMessage

getIdAndMessage :: Updates.Update -> Maybe (Word.Word64, EB.Event Message.Message)
getIdAndMessage upd = do
  m <- Updates.getMessage upd
  let chatId = Chat.getChatId . Message.getChat $ m
  return (chatId, EB.MessageEvent m)

-- getIdAndCallback :: Updates.Update -> Maybe Message.Message
getIdAndCallback :: Updates.Update -> Maybe (Word.Word64, T.Text, EB.Event a)
getIdAndCallback upd = do
  cb <- Updates.getCallBack upd
  m <- CallBackQuery.getMessage cb
  let chatId = Chat.getChatId . Message.getChat $ m
  let callbackId = CallBackQuery.getCallbackId cb
  data' <- CallBackQuery.getData cb
  return (chatId, callbackId, EB.SetRepetitionCountEvent (fromMaybe 1 $ (TR.readMaybe . T.unpack) data'))

-- TODO fix for InlineMenuCallbacks
showUpdateData :: Updates.Update -> T.Text
showUpdateData u =
  let
    msg = Updates.getMessage u
   in
    T.concat
      [ fromMaybe "[No name]" (getFirstName msg)
      , "("
      , fromMaybe "[No UserName]" (getUsername msg)
      , "):"
      , maybe "[No Text]" (\t -> "'" `T.append` t `T.append` "'") (getTextOfMsg u)
      ]

getTextOfMsg :: Updates.Update -> Maybe T.Text
getTextOfMsg = return <=< Message.getText <=< Updates.getMessage

getUser :: Maybe Message.Message -> Maybe User.User
getUser msg = do
  msg' <- msg
  Message.getSender msg'

getFirstName :: Maybe Message.Message -> Maybe T.Text
getFirstName msg = User.getFirstName <$> getUser msg

getUsername :: Maybe Message.Message -> Maybe T.Text
getUsername msg = User.getUserName <$> getUser msg

getMaxUpdateId :: [Updates.Update] -> Word.Word64
getMaxUpdateId = List.foldl' max 0 . fmap Updates.getUpdateId

sec :: Int
sec = 3

secToMicro :: Num a => a -> a
secToMicro = (* 1000000)