{-# LANGUAGE FlexibleInstances #-}
module Formatter
    ( Formatter
    , FormatterString
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
    , Data.ByteString.Char8.pack
    , Data.ByteString.Char8.getLine
    ) where

import Data.ByteString.Char8

type FormatterString = ByteString
data Alignment = LeftAlign | CenterAlign | RightAlign
class Formatter f where
    getDefaultColor :: f -> FormatterString
    wrapFgColor     :: FormatterString -> f -> f
    wrapBgColor     :: FormatterString -> f -> f
    underlineColor  :: FormatterString -> f -> f
    underline       :: f -> f
    align           :: Alignment -> f -> f
    bold            :: f -> f
    appendInner     :: FormatterString -> f -> f
    prependInner    :: FormatterString -> f -> f
    bare            :: f -> f
    monitor         :: FormatterString -> f -> f
    format          :: FormatterString -> f -> FormatterString

