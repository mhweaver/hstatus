{-# LANGUAGE OverloadedStrings #-}
module Segment.Time
    ( newTimeSegment
    ) where

import Segment
import Formatter
import Control.Concurrent
import Data.Time
import Data.Text

newTimeSegment :: Formatter f => f -> IO Segment
newTimeSegment formatter = do
    updateNotifier <- atomically newEmptyTMVar
    out <- atomically $ newTVar ""
    let segOut = SegmentOutput (updateNotifier, out)
    return Segment { runSegment = timeSegLoop segOut formatter
                   , getOutput = segOut }

timeSegLoop :: Formatter f => SegmentOutput -> f -> IO ()
timeSegLoop segOut formatter = do
    timezone <- getCurrentTimeZone
    currTime <- getCurrentTime
    let formattedTime = pack $ formatTime defaultTimeLocale "%I:%M:%S %P" (utcToLocalTime timezone currTime)
        SegmentOutput (notifier, out) = segOut
    atomically $ do
        writeTVar out $ format formatter formattedTime
        putTMVar notifier ()
    threadDelay $ 500 * 1000
    timeSegLoop segOut formatter
