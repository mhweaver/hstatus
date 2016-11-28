{-# LANGUAGE OverloadedStrings #-}
module Segment.Stdin
    ( newStdinSegment
    ) where

import Prelude hiding (getLine)
import Data.Text
import System.IO (isEOF)
import Segment
import Formatter
import Control.Monad
import Control.Concurrent

newStdinSegment :: Formatter f => MVar () -> f -> IO Segment
newStdinSegment dieChan formatter = do
    updateNotifier <- atomically newEmptyTMVar
    out <- atomically $ newTVar ""
    let segOut = SegmentOutput (updateNotifier, out)
    return Segment { runSegment = stdinLoop segOut dieChan formatter
                   , getOutput = segOut }

stdinLoop :: Formatter f => SegmentOutput -> MVar () -> f -> IO ()
stdinLoop segOut die formatter = do
    eof <- isEOF
    when eof $ putMVar die ()
    line <- getLine
    let SegmentOutput (notifier, out) = segOut
    atomically $ do
        writeTVar out $ format formatter line
        putTMVar notifier ()
    stdinLoop segOut die formatter
