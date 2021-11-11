{-# LANGUAGE OverloadedStrings #-}

module Logs where

import Control.Monad.Logger
  ( Loc
  , LogLevel(LevelDebug)
  , LogSource
  , LogStr
  , ToLogStr(toLogStr)
  , defaultLogStr
  , fromLogStr
  )
import qualified Data.ByteString.Char8 as BS
import Data.Time.LocalTime (getZonedTime)
import System.IO (stdout)

logLevelFilter :: LogLevel -> (LogSource -> LogLevel -> Bool)
logLevelFilter l _ l' = l <= l'

logOutput :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logOutput loc src level msg = do
  time <- getZonedTime
  let msg' =
        if level == LevelDebug
          then msg
          else (toLogStr . show) time <> " " <> msg
      ls = fromLogStr $ defaultLogStr loc src level msg'
  BS.hPutStr stdout ls
