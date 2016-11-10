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
    ) where

import Data.String
import Data.ByteString
import qualified Data.ByteString.Lazy as L
import Data.Text

class (Monoid s, IsString s) => FormatterString s where
instance FormatterString String
instance FormatterString ByteString
instance FormatterString L.ByteString
instance FormatterString Text

data Alignment = LeftAlign | CenterAlign | RightAlign
class Formatter f where
    getDefaultColor :: FormatterString s => f s -> s
    wrapFgColor     :: FormatterString s => s -> f s -> f s
    wrapBgColor     :: FormatterString s => s -> f s -> f s
    underlineColor  :: FormatterString s => s -> f s -> f s
    underline       :: FormatterString s => f s -> f s
    align           :: FormatterString s => Alignment -> f s -> f s
    bold            :: FormatterString s => f s -> f s
    appendInner     :: FormatterString s => s -> f s -> f s
    prependInner    :: FormatterString s => s -> f s -> f s
    bare            :: FormatterString s => f s -> f s
    monitor         :: FormatterString s => s -> f s -> f s
    format          :: FormatterString s => s -> f s -> s

