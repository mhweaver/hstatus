module StdinSegment
    ( StdinSegment
    , runSegment
    , newStdinSegment
    ) where

import Prelude hiding (getLine)
import System.IO ( isEOF )
import Segment
import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8

data StdinSegment = StdinSeg { getChan :: MVar ByteString
                             , die :: MVar Bool }
newStdinSegment :: MVar ByteString -> MVar Bool -> StdinSegment
newStdinSegment = StdinSeg

instance Segment StdinSegment where
    runSegment = stdinLoop

stdinLoop :: StdinSegment -> IO ()
stdinLoop seg = do
    eof <- isEOF
    when eof $ putMVar (die seg) True
    line <- getLine
    putMVar (getChan seg) line
    stdinLoop seg
