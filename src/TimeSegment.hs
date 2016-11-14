module TimeSegment
    ( TimeSegment
    , runSegment
    , newTimeSegment
    ) where

import Segment
import Formatter
import Data.ByteString.Char8
import Control.Concurrent
import Control.Monad
import Data.Time

data TimeSegment = TimeSegment { getChan :: MVar ByteString }
newTimeSegment :: MVar ByteString -> TimeSegment
newTimeSegment = TimeSegment

instance Segment TimeSegment where
    runSegment = timeSegLoop

timeSegLoop :: TimeSegment -> IO ()
timeSegLoop seg = do
    timezone <- getCurrentTimeZone
    currTime <- getCurrentTime
    let formattedTime = pack $ formatTime defaultTimeLocale "%I:%M:%S %P" (utcToLocalTime timezone currTime)
    putMVar (getChan seg) $ formattedTime
    threadDelay $ 60 * 1000
    timeSegLoop seg
