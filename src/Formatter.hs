{-# LANGUAGE FlexibleInstances #-}
module Formatter
    ( Formatter
    , module Data.Text
    , module Data.Text.IO
    , getDefaultColor
    , wrapFgColor
    , wrapBgColor
    , underlineColor
    , underline
    , align
    , Alignment (LeftAlign, CenterAlign, RightAlign)
    , bold
    , appendInner
    , prependInner
    , bare
    , monitor
    , format
    ) where

import Data.Text
import Data.Text.IO

data Alignment = LeftAlign | CenterAlign | RightAlign
class Formatter f where
    getDefaultColor :: f -> Text
    wrapFgColor     :: Text -> f -> f
    wrapBgColor     :: Text -> f -> f
    underlineColor  :: Text -> f -> f
    underline       :: f -> f
    align           :: Alignment -> f -> f
    bold            :: f -> f
    appendInner     :: Text -> f -> f
    prependInner    :: Text -> f -> f
    bare            :: f -> f
    monitor         :: Text -> f -> f
    format          :: f -> Text -> Text

