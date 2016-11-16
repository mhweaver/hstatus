{-# LANGUAGE OverloadedStrings #-}
module LoadSegment
    ( newLoadSegment
    ) where

import Segment
import Formatter
import Control.Concurrent
import System.Statgrab
import Text.Printf

data SegmentConfig f = LoadConfig { yellowThreshold :: Integer
                                  , redThreshold :: Integer
                                  , getBaseFormatter :: f
                                  , getRedFormatter :: f
                                  , getYellowFormatter :: f
                                  , getDefaultFormatter :: f
                                  }

newLoadSegment :: Formatter f => MVar Text -> f -> Segment
newLoadSegment chan formatter = Segment $ segmentLoop chan formatter

segmentLoop :: Formatter f => MVar Text -> f -> IO ()
segmentLoop out formatter = do
    host <- runStats (snapshot :: Stats Host)
    let bareFormatter = bare formatter
        config = LoadConfig { yellowThreshold = hostNCPU host
                            , redThreshold = 2 * hostNCPU host
                            , getBaseFormatter = formatter
                            , getRedFormatter = wrapFgColor "#ff0000" bareFormatter
                            , getYellowFormatter = wrapFgColor "#ffff00" bareFormatter
                            , getDefaultFormatter = wrapFgColor (getDefaultColor bareFormatter) bareFormatter
                            }
    runSegmentLoop out formatter config

runSegmentLoop :: Formatter f => MVar Text -> f -> SegmentConfig f -> IO ()
runSegmentLoop out formatter config = do
    load <- runStats (snapshot :: Stats Load)
    let output = format (renderOutput config load) formatter
    putMVar out output
    threadDelay $ 1000 * 1000 -- 1 second
    runSegmentLoop out formatter config

renderOutput :: Formatter f => SegmentConfig f -> Load -> Text
renderOutput config load = l1
               `mappend` " "
               `mappend` l5
               `mappend` " "
               `mappend` l15
    where l1 = renderSingleLoad config $ load1 load
          l5 = renderSingleLoad config $ load5 load
          l15 = renderSingleLoad config $ load15 load

renderSingleLoad :: Formatter f => SegmentConfig f -> Double -> Text
renderSingleLoad config load  = format loadStr formatter
    where formatter | load >= (fromIntegral $ redThreshold config)    = getRedFormatter config
                    | load >= (fromIntegral $ yellowThreshold config) = getYellowFormatter config
                    | otherwise                                       = getDefaultFormatter config
          loadStr = pack (printf "%.2f" load :: String)

