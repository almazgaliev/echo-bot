{-# LANGUAGE OverloadedStrings #-}

{- | The pure echo-bot logic module. It doesn't know anything about
 Telegram, other chat protocols, or any input/output. This is why we
 can easily test it.
-}
module EchoBot (
  makeState,
  respond,
  Event (..),
  Response (..),
  State,
  Handle (..),
  Config (..),
)
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Logger ((.<))
import qualified Logger

{- | The bot dependencies that the caller code should satisfy.

 Its @m@ parameter is a monad type where all actions are performed.
 Since the bot doesn't know what @m@ is, it cannot use arbitrary IO.
 Everything it can do with the monad is passed as handle fields,
 like 'hModifyState\''. For example, @m@ can be the 'IO' or 'State'
 monad.

 Its @a@ parameter is the message type. We don't impose any
 restrictions on the message type, hence the bot can support all
 possible message types, while 'hTextFromMessage' and
 'hMessageFromText' are implemented.
-}
data Handle m a = Handle
  { hLogHandle :: Logger.Handle m
  , hConfig :: Config
  , hGetState :: m State
  -- ^ Returns the current 'State'.
  , hModifyState' :: (State -> State) -> m ()
  -- ^ Updates the current state with applying the given function.
  -- You may find it similar to the 'modify'` method of the 'State'
  -- monad, as well as 'hGetState' is similar to 'get'.
  , hTextFromMessage :: a -> Maybe T.Text
  -- ^ Returns @Just text@ for a pure text message, so that the bot
  -- could try to parse it as a command. Otherwise it returns
  -- 'Nothing'.
  , hMessageFromText :: T.Text -> a
  -- ^ Constructs a message from a pure text. The bot can use it for
  -- creating a help text message.
  --
  -- Note that instead of adding 'hTextFromMessage' and
  -- 'hMessageFromText' into 'Handle', we might make them into
  -- methods of @Message@ type class. These two variants are very
  -- similar for our goals.
  }

-- | The initial configuration of the bot.
data Config = Config
  { confHelpReply :: T.Text
  -- ^ A reply to the @help@ command
  , confRepeatReply :: T.Text
  -- ^ A reply to the @repeat@ command. The string of @{count}@ in
  -- the text will be replaced with the current repetition count, so
  -- that you can use a template string like @"The new repetition
  -- count is {count}."@.
  , confRepetitionCount :: Int
  -- ^ The initial repetition count for echoing messages to start
  -- with.
  }

{- | An external event that the bot should process and respond to.
 It's parameterized with a message type.
-}
data Event a
  = -- | The user has sent a message.
    MessageEvent a
  | -- | Set the repetition count. This event is hidden from the
    --   outside code. The caller code cannot construct it, only get
    --   from a 'MenuResponse'.
    SetRepetitionCountEvent RepetitionCount
  deriving (Eq, Show)

{- | The bot response to an event. It's parameterized with a message
 type.
-}
data Response a
  = -- | A command to output a message to the user.
    MessageResponse a
  | -- | A command to output a menu with the given title and options.
    -- Each option is a pair of:
    --
    -- * a new repetition amount
    -- * an event that the caller code should sent to us if the user
    --   chooses this repetition count
    MenuResponse Title [(RepetitionCount, Event a)]
  deriving (Eq, Show)

type Title = T.Text

type RepetitionCount = Int

{- | An internal state of the bot.

 Note that we don't have to deal with multiple users here, we only
 keep the number of repetitions for a single user. Let the caller
 code be responsible for tracking multiple users and states for
 their bots.
-}
newtype State = State
  { stRepetitionCount :: RepetitionCount
  }

-- | Creates an initial, default bot state for a user.
makeState :: Config -> Either T.Text State
makeState conf = do
  checkConfig conf
  return State {stRepetitionCount = confRepetitionCount conf}

checkConfig :: Config -> Either T.Text ()
checkConfig conf =
  if confRepetitionCount conf < 0
    then Left "The repetition count must not be negative"
    else Right ()

-- | Evaluates responses for the passed event.
respond :: Monad m => Handle m a -> Event a -> m [Response a]
respond h (SetRepetitionCountEvent repetitionCount) =
  handleSettingRepetitionCount h repetitionCount
respond h (MessageEvent message)
  | isCommand h "/help" message = handleHelpCommand h
  | isCommand h "/repeat" message = handleRepeatCommand h
  | otherwise = respondWithEchoedMessage h message

isCommand :: Handle m a -> T.Text -> a -> Bool
isCommand h command message = case hTextFromMessage h message of
  Nothing -> False
  Just messageText -> command `T.isPrefixOf` messageText

handleHelpCommand :: Monad m => Handle m a -> m [Response a]
handleHelpCommand h = do
  Logger.logInfo (hLogHandle h) "Got the help command"
  let a = hMessageFromText h . confHelpReply . hConfig $ h
  return [MessageResponse a]

handleSettingRepetitionCount :: Monad m => Handle m a -> Int -> m [Response a]
handleSettingRepetitionCount h count = do
  Logger.logInfo (hLogHandle h) $ "The user has set the repetition count to " .< count
  let conf = hConfig h
  hModifyState' h (const $ State count)
  (State c) <- hGetState h
  let a = hMessageFromText h . T.replace "{count}" (T.pack . show $ c) . confRepeatReply $ conf
  return [MessageResponse a]

handleRepeatCommand :: Monad m => Handle m a -> m [Response a]
handleRepeatCommand h = do
  Logger.logInfo (hLogHandle h) "Got the repeat command"
  let variants = [1 .. 5]
  a <- hGetState h
  let count = stRepetitionCount a
  let message = "Current repetition count is: " ++ show count
  return [(MessageResponse . hMessageFromText h . T.pack) message, MenuResponse "choose repeat amount" (zip variants (SetRepetitionCountEvent <$> variants))]

respondWithEchoedMessage :: Monad m => Handle m a -> a -> m [Response a]
respondWithEchoedMessage h message = do
  Logger.logInfo (hLogHandle h) $
    "Echoing user input: " .< fromMaybe "<multimedia?>" (hTextFromMessage h message)
  state <- hGetState h
  let count = stRepetitionCount state
  return $ replicate count (MessageResponse message)
