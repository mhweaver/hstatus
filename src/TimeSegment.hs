{-# LANGUAGE OverloadedStrings #-}
module TimeSegment
    ( newTimeSegment
    ) where

import Segment
import Formatter
import Control.Concurrent
import Data.Time

newTimeSegment :: Formatter f => MVar FormatterString -> f -> Segment
newTimeSegment chan formatter = Segment $ timeSegLoop chan formatter

timeSegLoop :: Formatter f => MVar FormatterString -> f -> IO ()
timeSegLoop chan formatter = do
    timezone <- getCurrentTimeZone
    currTime <- getCurrentTime
    let formattedTime = pack $ formatTime defaultTimeLocale "%I:%M:%S %P" (utcToLocalTime timezone currTime)
    putMVar chan $ format formattedTime formatter
    threadDelay $ 1000 * 1000
    timeSegLoop chan formatter
