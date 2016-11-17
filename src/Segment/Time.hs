module Segment.Time
    ( newTimeSegment
    ) where

import Segment
import Formatter
import Control.Concurrent
import Data.Time
import Data.Text

newTimeSegment :: Formatter f => MVar Text -> f -> Segment
newTimeSegment chan formatter = Segment $ timeSegLoop chan formatter

timeSegLoop :: Formatter f => MVar Text -> f -> IO ()
timeSegLoop chan formatter = do
    timezone <- getCurrentTimeZone
    currTime <- getCurrentTime
    let formattedTime = pack $ formatTime defaultTimeLocale "%I:%M:%S %P" (utcToLocalTime timezone currTime)
    putMVar chan $ format formatter formattedTime
    threadDelay $ 1000 * 1000
    timeSegLoop chan formatter
