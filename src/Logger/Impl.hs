{-# LANGUAGE ImportQualifiedPost #-}

-- | The default implementation of the Logger interface.
module Logger.Impl (
  withHandle,
  Config (..),
)
where

import Control.Monad (when)
import Data.Text qualified as T
import Data.Text.IO (hPutStrLn)
import GHC.IO.Handle (hFlush)
import Logger qualified
import qualified GHC.IO.Handle.Types
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
logWith c level t = when (level >= confMinLevel c) $
 do
  let h = confHandle c
  hPutStrLn h $ T.pack (show level ++ " | ") `T.append` t
  hFlush h
