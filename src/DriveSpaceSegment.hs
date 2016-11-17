{-# LANGUAGE OverloadedStrings #-}
module DriveSpaceSegment
    ( newDriveSpaceSegment
    ) where

import Segment
import Formatter
import Control.Concurrent
import System.DiskSpace
import Text.Printf

newDriveSpaceSegment :: Formatter f => MVar Text -> f -> Segment
newDriveSpaceSegment chan formatter = Segment $ segLoop chan formatter

segLoop :: Formatter f => MVar Text -> f -> IO ()
segLoop out formatter = do
    diskUsage <- getDiskUsage "/"
    putMVar out $ format (renderOutput formatter diskUsage) formatter
    threadDelay $ 60 * 1000 * 1000 -- 1 minute
    segLoop out formatter

renderOutput :: Formatter f => f -> DiskUsage -> Text
renderOutput formatter usage = free `mappend` "GiB / " `mappend` total `mappend` "GiB (free)"
    where free = pack $ printf "%.2f" freeGiB
          total = pack $ printf "%.2f" totalGiB
          freeGiB = toGiB $ diskFree usage
          totalGiB = toGiB $ diskTotal usage
          toGiB b = (fromIntegral b) / 1024.0 / 1024.0 / 1024.0 :: Float

