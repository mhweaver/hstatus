{-# LANGUAGE OverloadedStrings #-}
module LemonbarFormatter
    ( module Formatter
    , newLemonbarFormatter
    ) where

import Formatter

data LemonbarFormatter = LBFormatter { getBefore :: Text
                                     , getAfter  :: Text }

wrapInner :: Text -> Text -> LemonbarFormatter -> LemonbarFormatter
wrapInner before after f = f { getBefore = getBefore f `mappend` before
                             , getAfter  = after       `mappend` getAfter f }
wrapOuter :: Text -> Text -> LemonbarFormatter -> LemonbarFormatter
wrapOuter before after f = f { getBefore = before     `mappend` getBefore f
                             , getAfter  = getAfter f `mappend` after }

instance Formatter LemonbarFormatter where
    getDefaultColor = const "-"
    wrapFgColor color = wrapOuter ("%{F" `mappend` color `mappend` "}") "%{F-}"
    wrapBgColor color = wrapOuter ("%{B" `mappend` color `mappend` "}") "%{B-}"
    underlineColor color = wrapOuter ("%{U" `mappend` color `mappend` "}") "%{U-}"
    underline = wrapOuter "%{+u}" "%{-u}"
    overline = wrapOuter "%{+o}" "%{-o}"
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
    format f s = getBefore f `mappend` s `mappend` getAfter f

newLemonbarFormatter :: LemonbarFormatter
newLemonbarFormatter = LBFormatter "" ""
