{-# LANGUAGE OverloadedStrings #-}
module DateSegment
    ( newDateSegment
    ) where

import Segment
import Formatter
import Control.Concurrent
import Data.Time
import Data.Text

newDateSegment :: Formatter f => MVar Text -> f -> Segment
newDateSegment chan formatter = Segment $ timeSegLoop chan formatter

timeSegLoop :: Formatter f => MVar Text -> f -> IO ()
timeSegLoop chan formatter = do
    currTime <- getCurrentTime
    let dateString = pack $ formatTime defaultTimeLocale "%a %_Y-%m-%d" currTime
    putMVar chan $ format dateString formatter
    threadDelay $ 30 * 60 * 1000 * 1000
    timeSegLoop chan formatter

