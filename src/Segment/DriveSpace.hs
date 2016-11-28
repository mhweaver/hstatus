{-# LANGUAGE OverloadedStrings #-}
module Segment.DriveSpace
    ( newDriveSpaceSegment
    ) where

import Segment
import Formatter
import Control.Concurrent
import System.DiskSpace
import Text.Printf

newDriveSpaceSegment :: Formatter f => f -> IO Segment
newDriveSpaceSegment formatter = do
    updateNotifier <- atomically newEmptyTMVar
    out <- atomically $ newTVar ""
    let segOut = SegmentOutput (updateNotifier, out)
    return Segment { runSegment = segLoop segOut formatter
                   , getOutput = segOut }

segLoop :: Formatter f => SegmentOutput -> f -> IO ()
segLoop segOut formatter = do
    diskUsage <- getDiskUsage "/"
    let SegmentOutput (notifier, out) = segOut
    atomically $ do
        writeTVar out . format formatter $ renderOutput formatter diskUsage
        putTMVar notifier ()
    threadDelay $ 60 * 1000 * 1000 -- 1 minute
    segLoop segOut formatter

renderOutput :: Formatter f => f -> DiskUsage -> Text
renderOutput formatter usage = free `mappend` "GiB / " `mappend` total `mappend` "GiB (free)"
    where free = pack $ printf "%.2f" freeGiB
          total = pack $ printf "%.2f" totalGiB
          freeGiB = toGiB $ diskFree usage
          totalGiB = toGiB $ diskTotal usage
          toGiB b = fromIntegral b / 1024.0 / 1024.0 / 1024.0 :: Float

