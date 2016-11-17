{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.IO (hFlush, stdout)
import Control.Monad
import Control.Concurrent
import Data.Text
import Formatter
import LemonbarFormatter
import Segment
import StdinSegment
import TimeSegment
import DateSegment
import MemorySegment
import LoadSegment
import CPUUsageSegment
import NetworkSegment
import DriveSpaceSegment
import Prelude hiding (putStrLn, concat)

-- Decorate an existing formatter by preprending a colored icon and underlining the whole thing in the same color
basicFormatter :: Formatter f => Text -> Text -> f -> f
basicFormatter color icon formatter = underline . underlineColor color . prependInner iconStr $ formatter
                                      where iconStr = format icon $ wrapFgColor color $ formatter

main :: IO ()
main = do
    outChannel <- newEmptyMVar -- Updates do this MVar are printed to stdout
    die        <- newEmptyMVar -- Semaphore to signal the main thread to terminate
    wakeUp     <- newEmptyMVar -- Semaphore to signal the watcher thread to check for updates

    let baseFormatter = newLemonbarFormatter

    stdinChannel <- newEmptyMVar
    timeChannel <- newEmptyMVar
    dateChannel <- newEmptyMVar
    memChannel  <- newEmptyMVar
    loadChannel <- newEmptyMVar
    cpuUsageChannel <- newEmptyMVar
    networkChannel <- newEmptyMVar
    driveSpaceChannel <- newEmptyMVar

    -- Fire up the segments (order doesn't matter here)
    sequence_ $ forkIO . runSegment <$>
        [ newStdinSegment stdinChannel die baseFormatter
        , newDateSegment dateChannel $ basicFormatter  "#00BAB1" "\61555 " baseFormatter -- \61555 = 
        , newTimeSegment timeChannel $ basicFormatter "#66BA00" "\61463 " baseFormatter -- \61463 = 
        , newMemorySegment memChannel $ basicFormatter "#BA4700" "\61642 " baseFormatter -- \61642 = 
        , newLoadSegment loadChannel $ basicFormatter "#0073BA" "\61568" baseFormatter -- \61568 = 
        , newCpuUsageSegment cpuUsageChannel $ basicFormatter "#0073BA" "\61568" baseFormatter -- \61568 = 
        , newNetworkSegment networkChannel $ basicFormatter "#008079" "\61672 " baseFormatter -- \61672 = 
        , newDriveSpaceSegment driveSpaceChannel $ basicFormatter "#999999" "\61600 " baseFormatter -- \61600 = 
        ]

    -- Set up watchers to notify the main watcher thread that something happened
    -- The segments will appear in the order listed here
    let leftChannels  = [ stdinChannel ]
        rightChannels = [ driveSpaceChannel
                        , networkChannel
                        , cpuUsageChannel
                        , loadChannel
                        , memChannel
                        , dateChannel
                        , timeChannel
                        ]
    sequence_ $ fmap
        (\chan -> forkIO $ forever (readMVar chan >> putMVar wakeUp ()))
        (leftChannels ++ rightChannels)

    -- Watch the channels for updates
    let initialLefts  = fmap (\seg -> (seg, "")) leftChannels
        initialRights = fmap (\seg -> (seg, "")) rightChannels
    forkIO $ watchForUpdates
                initialLefts
                initialRights
                outChannel
                wakeUp
                ""
                (align LeftAlign baseFormatter)
                (align RightAlign baseFormatter)

    -- Watch the output channel and print to stdout, flushing after every write
    forkIO . forever $ takeMVar outChannel >>= putStrLn >> hFlush stdout

    -- Await termination signal
    void $ takeMVar die

watchForUpdates :: Formatter f =>
                   [(MVar Text, Text)] -- (segment output channel, prev output from channel)
                -> [(MVar Text, Text)] -- Right channels
                -> MVar Text                      -- Output channel
                -> MVar ()                                   -- Wake up semaphore
                -> Text                           -- Previously outputted value
                -> f                                         -- Left align formatter
                -> f                                         -- Right align formatter
                -> IO ()
watchForUpdates lefts rights out wakeUp oldOut leftF rightF = do
    takeMVar wakeUp
    let getUpdatedOutputs = fmap (\(chan, oldValue) -> do
                                      maybeUpdate <- tryTakeMVar chan
                                      return $ case maybeUpdate of
                                          Nothing -> (chan, oldValue)
                                          Just d -> (chan, d))
    updatedLefts  <- sequence $ getUpdatedOutputs lefts
    updatedRights <- sequence $ getUpdatedOutputs rights
    let leftOut  = intercalate "  " . fmap snd $ updatedLefts
        rightOut = intercalate "  " . fmap snd $ updatedRights
        outString = format leftOut leftF `mappend` "  " `mappend` format rightOut rightF

    when (outString /= oldOut) $ -- Only output if something changed
        putMVar out outString

    watchForUpdates updatedLefts updatedRights out wakeUp outString leftF rightF

