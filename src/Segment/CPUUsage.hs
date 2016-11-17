{-# LANGUAGE OverloadedStrings #-}
module Segment.CPUUsage
    ( newCpuUsageSegment
    ) where

import Segment
import Formatter hiding (zip, filter, find)
import Control.Concurrent
import Text.Printf
import Data.Text.ICU
import Data.Maybe
import Data.Either
import Data.Text.Read

import Prelude hiding (readFile, putStrLn, lines, words, unwords)
import Control.Applicative hiding (optional)

data SegmentConfig f = Config { redFg :: f
                              , orangeFg :: f
                              , defaultFg :: f
                              }

newCpuUsageSegment :: Formatter f => MVar Text -> f -> Segment
newCpuUsageSegment chan formatter = Segment $ runUsageLoop chan formatter config
    where bareFormatter = bare formatter
          config = Config { redFg = wrapFgColor "#aa0000" bareFormatter
                          , orangeFg = wrapFgColor "#ee7600" bareFormatter
                          , defaultFg = wrapFgColor (getDefaultColor bareFormatter) bareFormatter
                          }

runUsageLoop :: Formatter f => MVar Text -> f -> SegmentConfig f -> IO ()
runUsageLoop out formatter config = do
    samples0 <- getSamples
    threadDelay $ 3 * 1000 * 1000 -- 3 seconds
    samples1 <- getSamples
    let percentages = getPercentages $ zip samples0 samples1
    putMVar out . format formatter $ renderOutput config percentages
    runUsageLoop out formatter config

renderOutput :: Formatter f => SegmentConfig f -> [Float] -> Text
renderOutput config percentages = unwords $ fmap renderSinglePercentage percentages
    where pFormatter p | p >= 85.0 = redFg config
                       | p >= 50.0 = orangeFg config
                       | otherwise = defaultFg config
          renderSinglePercentage p = format (pFormatter p) . pack $ printf "%.2f" p ++ "%"

data Sample = Sample Integer Integer -- (idle, total)
getSamples :: IO ([Sample])
getSamples = do
    rawInput <- readFile "/proc/stat"
    let samples = fmap toSample . fmap toIntegers . filter isCpuLine . lines $ rawInput
    return $ samples

isCpuLine :: Text -> Bool
isCpuLine str = isJust match
    where re = regex [] "^cpu[0-9] "
          match = find re str

toIntegers :: Text -> [Integer]
toIntegers str = fmap fst . rights . fmap (decimal :: Reader Integer) . words $ str

toSample :: [Integer] -> Sample
toSample all@(user:nice:system:idle:rest) = Sample idle (sum all)

getPercentages :: [(Sample, Sample)] -> [Float]
getPercentages samples = fmap toPercent samples
    where toPercent ((Sample i0 t0), (Sample i1 t1)) = 100.0 * fromIntegral ((t1 - t0) - (i1 - i0)) / fromIntegral (t1 - t0)
