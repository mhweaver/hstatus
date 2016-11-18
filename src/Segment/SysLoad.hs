{-# LANGUAGE OverloadedStrings #-}
module Segment.SysLoad
    ( newLoadSegment
    ) where

import Segment
import Formatter hiding (find, filter, length)
import Control.Concurrent
import Data.Maybe
import Data.Text.ICU
import Data.Text.Read
import Text.Printf
import Prelude hiding (readFile, words, lines)

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
    cpuInfo <- getCpuInfo
    let bareFormatter = bare formatter
        config = LoadConfig { yellowThreshold = numCpus cpuInfo
                            , redThreshold = 2 * numCpus cpuInfo
                            , getBaseFormatter = formatter
                            , getRedFormatter = wrapFgColor "#ff0000" bareFormatter
                            , getYellowFormatter = wrapFgColor "#ffff00" bareFormatter
                            , getDefaultFormatter = wrapFgColor (getDefaultColor bareFormatter) bareFormatter
                            }
    runSegmentLoop out formatter config

data CPUInfo = CPUInfo { numCpus :: Integer }
getCpuInfo :: IO CPUInfo
getCpuInfo = do
  rawCpuInfo <- readFile "/proc/cpuinfo"
  return . CPUInfo . fromIntegral . length . filter isProcessorLine . lines $ rawCpuInfo

isProcessorLine :: Text -> Bool
isProcessorLine line = isJust $ find re line
    where re = regex [] "^processor\\s+: \\d+"

runSegmentLoop :: Formatter f => MVar Text -> f -> SegmentConfig f -> IO ()
runSegmentLoop out formatter config = do
    load <- getLoad
    let output = format formatter $ renderOutput config load
    putMVar out output
    threadDelay $ 1000 * 1000 -- 1 second
    runSegmentLoop out formatter config

data Load = Load { load1 :: Double
                 , load5 :: Double
                 , load15 :: Double }

getLoad :: IO Load
getLoad = do
    loadAvg <- readFile "/proc/loadavg"
    return . extractLoad . words $ loadAvg

extractLoad :: [Text] -> Load
extractLoad (l1:l5:l15:_) = Load (toDouble l1) (toDouble l5) (toDouble l15)
    where toDouble t = case double t of
                       Right d -> fst d
                       Left _  -> -1
extractLoad _ = Load (-1) (-1) (-1)

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
renderSingleLoad config load  = format formatter loadStr
    where formatter | load >= fromIntegral (redThreshold config)    = getRedFormatter config
                    | load >= fromIntegral (yellowThreshold config) = getYellowFormatter config
                    | otherwise                                       = getDefaultFormatter config
          loadStr = pack (printf "%.2f" load :: String)

