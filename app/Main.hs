{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import LemonbarFormatter

main :: IO ()
main = do
    let formatter = newLemonbarFormatter "red"
    putStrLn $ format "test1" . underline $ formatter
