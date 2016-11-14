{-# LANGUAGE ExistentialQuantification #-}
module Segment
    ( Formatter
    , Segment
    , Seg
    , segment
    , runSegment
    ) where

import Formatter
import Control.Concurrent
import Data.String

class Segment s where
    runSegment :: s -> IO ()

data Seg = forall a . Segment a => MkSeg a
segment :: Segment a => a -> Seg
segment = MkSeg

instance Segment Seg where
    runSegment (MkSeg a) = runSegment a
