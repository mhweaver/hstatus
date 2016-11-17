module StdinSegment
    ( newStdinSegment
    ) where

import Prelude hiding (getLine)
import Data.Text
import System.IO (isEOF)
import Segment
import Formatter
import Control.Monad
import Control.Concurrent

newStdinSegment :: Formatter f => MVar Text -> MVar () -> f -> Segment
newStdinSegment outChan dieChan formatter = Segment $ stdinLoop outChan dieChan formatter

stdinLoop :: Formatter f => MVar Text -> MVar () -> f -> IO ()
stdinLoop out die formatter = do
    eof <- isEOF
    when eof $ putMVar die ()
    line <- getLine
    putMVar out $ format formatter line
    stdinLoop out die formatter
