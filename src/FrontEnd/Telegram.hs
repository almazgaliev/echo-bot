{-# LANGUAGE ImportQualifiedPost #-}

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
import Data.Map qualified as DM
import EchoBot qualified as EB
import Network.HTTP.Client (Manager)

type APIToken = String

data Handle m a = Handle
  { hBotHandle :: DM.Map Integer (EB.Handle m a)
  , hManager :: Manager
  , hToken :: APIToken
  }

run :: Handle IO a -> IO ()
run handle = do
  do
    -- threadDelay $ sec * 1000000
    -- run handle
    return $ error "Not Implemented"


run' :: Int -> IO ()
run' i = do
  do
    print i
    -- TIO.putStrLn "Welcome to the echo-bot!"
    -- let m = hBotHandle handle
    threadDelay $ sec * 1000000
    run' (i+1)

sec :: Int
sec = 5