{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import           Lib

main :: IO ()
main = print (tokenizeSentence "if kladjf")
