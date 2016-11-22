{-# LANGUAGE OverloadedStrings #-}
module Segment.Memory
    ( newMemorySegment
    ) where

import Segment
import Formatter hiding (foldl')
import Control.Concurrent
import Text.Printf
import Prelude hiding (readFile, lines, words)
import Data.Text.Read
import Data.Either
import Data.Maybe
import Data.Foldable

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
    memInfo <- getMemInfo
    let total  = memTotal memInfo
        used   = total - (memFree memInfo + cached memInfo + buffers memInfo)
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
          usedGiB =  (fromIntegral used / 1024.0 / 1024.0) :: Float
          totalGiB = (fromIntegral total / 1024.0 / 1024.0) :: Float
          usedGiBStr  = pack $ (printf "%.2f" usedGiB :: String) ++ "GiB"
          totalGiBStr = pack $ (printf "%.2f" totalGiB :: String) ++ "GiB"
          percentUsedStr = pack $ (printf "%.2f" percentUsed :: String) ++ "%"
          formattedUsed = format usageFormatter usedGiBStr
          formattedPercentUsed = format usageFormatter percentUsedStr

data MemInfo = MemInfo { memTotal :: Integer
                       , memFree :: Integer
                       , buffers :: Integer
                       , cached :: Integer
                       }

getMemInfo :: IO MemInfo
getMemInfo = do
    rawMemInfo <- readFile "/proc/meminfo"
    return $ parseMemInfo rawMemInfo

parseMemInfo :: Text -> MemInfo
parseMemInfo = parseMemLines . lines

parseMemLines :: [Text] -> MemInfo
parseMemLines = foldl' parseMemLine (MemInfo 0 0 0 0)

parseMemLine :: MemInfo -> Text -> MemInfo
parseMemLine info memLine | "MemTotal:" `isPrefixOf` memLine = info { memTotal = getValue memLine }
                          | "MemFree:"  `isPrefixOf` memLine = info { memFree  = getValue memLine }
                          | "Buffers:"  `isPrefixOf` memLine = info { buffers  = getValue memLine }
                          | "Cached:"   `isPrefixOf` memLine = info { cached   = getValue memLine }
                          | otherwise                        = info

getValue :: Text -> Integer
getValue = fromMaybe 0 . safeHead . toIntegers

toIntegers :: Text -> [Integer]
toIntegers = fmap fst . rights . fmap (decimal :: Reader Integer) . words

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead [] = Nothing
