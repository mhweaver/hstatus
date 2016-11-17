{-# LANGUAGE OverloadedStrings #-}
module Segment.Memory
    ( newMemorySegment
    ) where

import Segment
import Formatter
import Control.Concurrent
import System.Statgrab
import Text.Printf

data SegmentConfig f = SegConfig {
                               getBaseFormatter      :: f
                               , getLowFormatter     :: f
                               , getHighFormatter    :: f
                               , getHigherFormatter  :: f
                               , getHighestFormatter :: f
                               }
newMemorySegment :: Formatter f => MVar Text -> f -> Segment
newMemorySegment chan formatter = Segment $ memSegmentLoop chan config
    where bareFormatter = bare formatter
          config = SegConfig
            { getBaseFormatter    = formatter
            , getLowFormatter     = wrapFgColor (getDefaultColor bareFormatter) bareFormatter
            , getHighFormatter    = wrapFgColor "#fff600" bareFormatter
            , getHigherFormatter  = wrapFgColor "#ffae00" bareFormatter
            , getHighestFormatter = wrapFgColor "#ff0000" bareFormatter
            }

memSegmentLoop :: Formatter f => MVar Text -- Output channel
                              -> SegmentConfig f
                              -> IO ()
memSegmentLoop out config = do
    stats <- runStats (snapshot :: Stats Memory)
    let total  = memTotal stats
        used   = total - (memFree stats + memCache stats)
        output = renderOutput used total config
    putMVar out output
    threadDelay $ 5 * 1000 * 1000 -- 5 seconds
    memSegmentLoop out config

renderOutput :: Formatter f => Integer -> Integer -> SegmentConfig f -> Text
renderOutput used total config = format (getBaseFormatter config) $
                                          formattedUsed
                                          `mappend` " / "
                                          `mappend` totalGiBStr
                                          `mappend` " ("
                                          `mappend` formattedPercentUsed
                                          `mappend` ")"
    where percentUsed = (100.0 * fromIntegral used / fromIntegral total) :: Float
          usageFormatter | percentUsed > 95.0 = getHighestFormatter config
                         | percentUsed > 90.0 = getHigherFormatter  config
                         | percentUsed > 80.0 = getHighFormatter    config
                         | otherwise          = getLowFormatter     config
          usedGiB =  (fromIntegral used  / 1024.0 / 1024.0 / 1024.0) :: Float
          totalGiB = (fromIntegral total / 1024.0 / 1024.0 / 1024.0) :: Float
          usedGiBStr  = pack $ (printf "%.2f" usedGiB :: String) ++ "GiB"
          totalGiBStr = pack $ (printf "%.2f" totalGiB :: String) ++ "GiB"
          percentUsedStr = pack $ (printf "%.2f" percentUsed :: String) ++ "%"
          formattedUsed = format usageFormatter usedGiBStr
          formattedPercentUsed = format usageFormatter percentUsedStr

