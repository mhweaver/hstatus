module TimeSegment
    ( TimeSegment
    , newTimeSegment
    ) where

import Segment
import Formatter
import Data.ByteString.Char8
import Control.Concurrent
import Control.Monad
import Data.Time

data TimeSegment = TimeSegment { getChan :: MVar ByteString }
newTimeSegment :: MVar ByteString -> Segment
newTimeSegment chan = Segment $ timeSegLoop $ TimeSegment chan

timeSegLoop :: TimeSegment -> IO ()
timeSegLoop seg = do
    timezone <- getCurrentTimeZone
    currTime <- getCurrentTime
    let formattedTime = pack $ formatTime defaultTimeLocale "%I:%M:%S %P" (utcToLocalTime timezone currTime)
    putMVar (getChan seg) $ formattedTime
    threadDelay $ 60 * 1000
    timeSegLoop seg
