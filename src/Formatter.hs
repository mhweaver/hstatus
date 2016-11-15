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
    , Data.Text.pack
    , Data.Text.intercalate
    , Data.Text.IO.getLine
    , Data.Text.IO.putStrLn
    ) where

import Data.Text
import Data.Text.IO

type FormatterString = Text
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

