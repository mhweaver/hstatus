{-# LANGUAGE OverloadedStrings #-}
module Segment.Network
    ( newNetworkSegment
    ) where

import Segment
import Formatter hiding (find, filter, head)
import Control.Concurrent
import Data.Maybe
import Data.Either
import Prelude hiding (readFile, lines, words)
import Data.Text.ICU
import Data.Text.Read
import Text.Printf

data Sample = InitialSample | Sample { rxBytes :: !Integer, txBytes :: !Integer }
data Icons = Icons { rxIcon :: Text, txIcon :: Text }

newNetworkSegment :: Formatter f => f -> IO Segment
newNetworkSegment formatter = do
    updateNotifier <- atomically newEmptyTMVar
    out <- atomically $ newTVar ""
    let whiteFg = format (wrapFgColor "#ffffff" $ bare formatter)
        icons = Icons { rxIcon = whiteFg "\61677" -- \61677 = 
                      , txIcon = whiteFg "\61678" } -- \61678 = 
        segOut = SegmentOutput (updateNotifier, out)
    return Segment { runSegment = networkSegLoop segOut formatter icons InitialSample
                   , getOutput = segOut }

interval :: Int
interval = 2

networkSegLoop :: Formatter f => SegmentOutput -> f -> Icons -> Sample -> IO ()
networkSegLoop segOut formatter icons lastSample = do
    sample <- getSample
    let SegmentOutput (notifier, out) = segOut
    atomically $ do
        writeTVar out $ format formatter (renderOutput icons lastSample sample)
        putTMVar notifier ()
    threadDelay $ interval * 1000 * 1000 -- 2 seconds
    networkSegLoop segOut formatter icons sample

renderOutput :: Icons -> Sample -> Sample -> Text
renderOutput _ InitialSample _ = "Unknown"
renderOutput _ _ InitialSample = "Unknown"
renderOutput (Icons rxIcon txIcon) s0 s1 = output
    where rxSpeed = toStr rxSpeedBps
          txSpeed = toStr txSpeedBps
          output = rxIcon `mappend` rxSpeed `mappend` " " `mappend` txIcon `mappend` txSpeed
          rxSpeedBps = fromIntegral (rxBytes s1 - rxBytes s0) / fromIntegral interval :: Float
          txSpeedBps = fromIntegral (txBytes s1 - txBytes s0) / fromIntegral interval :: Float
          toStr bps = pack (printf "%.1f KiB/s" $ bps/1024.0)

getSample :: IO Sample
getSample = do
    rawInput <- readFile "/proc/net/dev"
    let sample = toSample . toIntegers . head . filter isIFaceLine . lines $ rawInput
    return sample

isIFaceLine :: Text -> Bool
isIFaceLine line = isJust $ find re line
    where re = regex [] "^\\s+eth0:"

toIntegers :: Text -> [Integer]
toIntegers = fmap fst . rights . fmap (decimal :: Reader Integer) . words

toSample :: [Integer] -> Sample
toSample(rx:_:_:_:_:_:_:_:tx:_) = Sample { rxBytes = rx, txBytes = tx }
toSample _ = InitialSample
