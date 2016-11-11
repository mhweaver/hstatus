{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8
import LemonbarFormatter
import StdinSegment
import Prelude hiding (putStrLn, concat)

main :: IO ()
main = do
    let baseFormatter = newLemonbarFormatter "-"
    die        <- newEmptyMVar -- Anything getting put in this MVar causes the program to terminate
    outChannel <- newEmptyMVar -- Updates do this MVar are printed to stdout

    -- Create segments
    stdinChannel <- newEmptyMVar
    let stdinSeg = newStdinSegment stdinChannel die

    let segments = [ stdinSeg ]
    let channels = [ stdinChannel ]

    -- Fire up the segments
    sequence_ $ fmap (forkIO . runSegment) segments

    -- Watch the channels for updates
    wakeUp <- newEmptyMVar -- Signals the watcher thread to check for updates
    -- Set up watchers to notify the main watcher thread that something happened
    sequence_ $ fmap (\chan -> forkIO $ forever (readMVar chan >> putMVar wakeUp True)) channels
    forkIO $ watchForUpdates (fmap (\seg -> (seg, "")) channels) outChannel wakeUp

    -- Watch the output channel and print to stdout
    forkIO . forever $ takeMVar outChannel >>= putStrLn

    -- Await termination signal
    void $ takeMVar die

watchForUpdates :: [(MVar ByteString, ByteString)] -- (segment output channel, prev output from channel)
                -> MVar ByteString                 -- Output channel
                -> MVar Bool                       -- Wake up semaphore
                -> IO ()
watchForUpdates channels outChannel wakeUp = do
    takeMVar wakeUp
    updatedChannels <- sequence $ fmap (\(chan, oldValue) -> do
                                      maybeUpdate <- tryTakeMVar chan
                                      return $ case maybeUpdate of
                                          Nothing -> (chan, oldValue)
                                          Just d -> (chan, d)) channels :: IO [(MVar ByteString, ByteString)]
    let oldOutString = concat . fmap snd $ channels
        outString    = concat . fmap snd $ updatedChannels
    when (outString /= oldOutString) $ -- Only output if something changed
        putMVar outChannel outString
    watchForUpdates updatedChannels outChannel wakeUp

