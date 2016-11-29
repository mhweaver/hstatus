module Segment
    ( Segment (Segment)
    , SegmentOutput (SegmentOutput)
    , runSegment
    , getOutput
    , module Control.Concurrent.STM
    , threadDelay
    ) where

import Control.Concurrent.STM
import Control.Concurrent
import Data.Text

newtype SegmentOutput = SegmentOutput (TMVar (), TVar Text) -- (Update notifier, Output)
data Segment = Segment { runSegment :: IO ()
                       , getOutput :: SegmentOutput
                       }
