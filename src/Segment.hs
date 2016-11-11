module Segment
    ( Formatter
    , Segment
    , runSegment
    ) where

import Formatter
import Control.Concurrent
import Data.String

class Segment s where
    runSegment :: s -> IO ()

