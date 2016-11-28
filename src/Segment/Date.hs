{-# LANGUAGE OverloadedStrings #-}
module Segment.Date
    ( newDateSegment
    ) where

import Segment
import Formatter
import Data.Time
import Data.Text

newDateSegment :: Formatter f => f -> IO Segment
newDateSegment formatter = do
    updateNotifier <- atomically newEmptyTMVar
    out <- atomically $ newTVar ""
    let segOut = SegmentOutput (updateNotifier, out)
    return Segment { runSegment = timeSegLoop segOut formatter
                   , getOutput = segOut
                   }

timeSegLoop :: Formatter f => SegmentOutput -> f -> IO ()
timeSegLoop segOut formatter = do
    currTime <- getCurrentTime
    let dateString = pack $ formatTime defaultTimeLocale "%a %_Y-%m-%d" currTime
        SegmentOutput (notifier, out) = segOut
    atomically $ do
        writeTVar out $ format formatter dateString
        putTMVar notifier ()
    threadDelay $ 30 * 60 * 1000 * 1000
    timeSegLoop segOut formatter

