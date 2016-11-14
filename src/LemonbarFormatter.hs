{-# LANGUAGE OverloadedStrings #-}
module LemonbarFormatter
    ( Formatter
    , LemonbarFormatter
    , newLemonbarFormatter
    , getDefaultColor
    , wrapFgColor
    , wrapBgColor
    , underlineColor
    , underline
    , align
    , bold
    , appendInner
    , prependInner
    , bare
    , monitor
    , format
    ) where

import Formatter

data LemonbarFormatter = LBFormatter { getBefore :: FormatterString
                                     , getAfter  :: FormatterString }

wrapInner :: FormatterString -> FormatterString -> LemonbarFormatter -> LemonbarFormatter
wrapInner before after f = f { getBefore = getBefore f `mappend` before
                             , getAfter  = after       `mappend` getAfter f }
wrapOuter :: FormatterString -> FormatterString -> LemonbarFormatter -> LemonbarFormatter
wrapOuter before after f = f { getBefore = before     `mappend` getBefore f
                             , getAfter  = getAfter f `mappend` after }

instance Formatter LemonbarFormatter where
    getDefaultColor = const "-"
    wrapFgColor color = wrapOuter ("%F" `mappend` color `mappend` "}") "%{F-}"
    wrapBgColor color = wrapOuter ("%B" `mappend` color `mappend` "}") "%{B-}"
    underlineColor color = wrapOuter ("%U" `mappend` color `mappend` "}") "%{U-}"
    underline = wrapOuter "%{+u}" "%{-u}"
    align alignment = wrapOuter ("%{" `mappend` loc `mappend` "}") "" where
                      loc = case alignment of
                            LeftAlign -> "l"
                            CenterAlign -> "c"
                            RightAlign -> "r"
    bold = id -- Unsupported
    appendInner = wrapInner ""
    prependInner toPrepend = wrapInner toPrepend ""
    bare f = f { getBefore = "", getAfter = "" }
    monitor m =  wrapOuter ("%{S" `mappend` m `mappend` "}") ""
    format s f = getBefore f `mappend` s `mappend` getAfter f

newLemonbarFormatter :: LemonbarFormatter
newLemonbarFormatter = LBFormatter "" ""
