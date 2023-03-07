{-# LANGUAGE OverloadedStrings #-}

-- | The default implementation of the Logger interface.
module Logger.Impl (
  withHandle,
  Config (..),
)
where

import Control.Monad (when)
import qualified Data.Text as T
import Data.Text.IO (hPutStrLn)
import Data.Time (ZonedTime, getZonedTime)
import GHC.IO.Handle (hFlush)
import qualified GHC.IO.Handle.Types
import qualified Logger

data Config = Config
  { confHandle :: GHC.IO.Handle.Types.Handle
  -- ^ A file handle to output formatted log messages to with
  -- 'System.IO.hPutStrLn' or 'Data.Text.IO.hPutStrLn'. For example,
  -- it might be 'System.IO.stderr' or a handle of a regular open
  -- file.
  , confMinLevel :: Logger.Level
  -- ^ The minimum level of a printable log message. Messages with
  -- lower levels should not be printed.
  }

withHandle :: Config -> (Logger.Handle IO -> IO ()) -> IO ()
withHandle config f = f Logger.Handle {Logger.hLowLevelLog = logWith config}

logWith :: Config -> Logger.Level -> T.Text -> IO ()
logWith c level msg = when (level >= confMinLevel c) $
  do
    time <- getZonedTime
    let h = confHandle c
    let row = [showTime time, showLevel level, msg]
    hPutStrLn h $ T.intercalate " | " row
    hFlush h

showTime :: ZonedTime -> T.Text
showTime = T.pack . take 19 . show

showLevel :: Logger.Level -> T.Text
showLevel level = T.justifyRight 7 ' ' (T.pack (show level))