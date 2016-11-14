{-# LANGUAGE ExistentialQuantification #-}
module Segment
    ( Segment (Segment)
    , runSegment
    ) where

import Formatter
import Control.Concurrent
import Data.String

data Segment = Segment { runSegment :: IO () }
