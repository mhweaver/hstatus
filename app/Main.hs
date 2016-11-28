{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hFlush, stdout)
import Control.Monad
import Control.Concurrent
import Data.Text
import LemonbarFormatter
import Segment
import Segment.Segments
import Prelude hiding (putStrLn, concat)

-- Decorate an existing formatter by preprending a colored icon and underlining the whole thing in the same color
basicFormatter :: Formatter f => Text -> Text -> f -> f
basicFormatter color icon formatter = overline . underlineColor color . prependInner iconStr $ formatter
                                      where iconStr = format (wrapFgColor color formatter) icon

main :: IO ()
main = do
    outChannel <- newEmptyMVar -- Updates do this MVar are printed to stdout
    die        <- newEmptyMVar -- Semaphore to signal the main thread to terminate
    wakeUp     <- newEmptyMVar -- Semaphore to signal the watcher thread to check for updates

    let baseFormatter = newLemonbarFormatter

    -- Build segments
    stdinSegment <- newStdinSegment die baseFormatter
    dateSegment <- newDateSegment $ basicFormatter  "#00BAB1" "\61555 " baseFormatter -- \61555 = 
    timeSegment <- newTimeSegment $ basicFormatter "#66BA00" "\61463 " baseFormatter -- \61463 = 
    memSegment <- newMemorySegment $ basicFormatter "#BA4700" "\61642 " baseFormatter -- \61642 = 
    loadSegment <- newLoadSegment $ basicFormatter "#0073BA" "\61568 " baseFormatter -- \61568 = 
    cpuUsageSegment <- newCpuUsageSegment $ basicFormatter "#0073BA" "\61568 " baseFormatter -- \61568 = 
    networkSegment <- newNetworkSegment $ basicFormatter "#008079" "\61672 " baseFormatter -- \61672 = 
    driveSpaceSegment <- newDriveSpaceSegment $ basicFormatter "#999999" "\61600 " baseFormatter -- \61600 = 

    -- The segments will appear in the order listed here
    let leftSegments  = [ stdinSegment ]
        rightSegments = [ driveSpaceSegment
                        , networkSegment
                        , cpuUsageSegment
                        , loadSegment
                        , memSegment
                        , dateSegment
                        , timeSegment
                        ]

    -- Fire up the segments
    sequence_ $ forkIO . runSegment <$> leftSegments ++ rightSegments

    -- Set up watchers to notify the main watcher thread that something happened
    let watchSegment seg = forkIO . forever $ atomically (takeTMVar notifier) >> tryPutMVar wakeUp ()
            where SegmentOutput (notifier, _) = getOutput seg
    sequence_ . fmap watchSegment $ leftSegments ++ rightSegments

    -- Watch the channels for updates
    let formatters = Formatters { leftF = align LeftAlign baseFormatter
                                , rightF = align RightAlign baseFormatter
                                , firstScreen = monitor "f" baseFormatter
                                , lastScreen = monitor "l" baseFormatter }
    forkIO $ watchForUpdates
                leftSegments
                rightSegments
                outChannel
                wakeUp
                formatters

    -- Watch the output channel and print to stdout, flushing after every write
    forkIO . forever $ takeMVar outChannel >>= putStrLn >> hFlush stdout

    -- Await termination signal
    void $ takeMVar die

data Formatters f = Formatters { leftF :: f
                               , rightF :: f
                               , firstScreen :: f
                               , lastScreen :: f
                               }

watchForUpdates :: Formatter f => [Segment]    -- Left segments
                               -> [Segment]    -- Right segments
                               -> MVar Text    -- Output channel
                               -> MVar ()      -- Wake up semaphore
                               -> Formatters f -- Formatters (duh)
                               -> IO ()
watchForUpdates lefts rights out wakeUp fs = do
    takeMVar wakeUp
    let getUpdatedOutputs = fmap (\seg -> atomically $ do
                                          let SegmentOutput (_, out) = getOutput seg
                                          readTVar out)
    updatedLefts  <- sequence $ getUpdatedOutputs lefts
    updatedRights <- sequence $ getUpdatedOutputs rights
    let leftOut  = intercalate "  " updatedLefts
        rightOut = intercalate "  " updatedRights
        outString = format (leftF fs) leftOut `mappend` "  " `mappend` format (rightF fs) rightOut
        outToBothMonitors = format (firstScreen fs) outString `mappend` format (lastScreen fs) outString
    putMVar out outToBothMonitors

    watchForUpdates lefts rights out wakeUp fs

