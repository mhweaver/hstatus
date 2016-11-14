module StdinSegment
    ( StdinSegment
    , newStdinSegment
    ) where

import Prelude hiding (getLine)
import System.IO ( isEOF )
import Segment
import Control.Monad
import Control.Concurrent
import Data.ByteString.Char8

data StdinSegment = StdinSeg { getChan :: MVar ByteString
                             , die :: MVar () }
newStdinSegment :: MVar ByteString -> MVar () -> Segment
newStdinSegment outChan dieChan = Segment . stdinLoop $ StdinSeg outChan dieChan

stdinLoop :: StdinSegment -> IO ()
stdinLoop seg = do
    eof <- isEOF
    when eof $ putMVar (die seg) ()
    line <- getLine
    putMVar (getChan seg) line
    stdinLoop seg
