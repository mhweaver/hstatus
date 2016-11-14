{-# LANGUAGE ExistentialQuantification #-}
module Segment
    ( Segment (Segment)
    , runSegment
    ) where

data Segment = Segment { runSegment :: IO () }
