module Main where

import           Lib

main :: IO ()
main = print (tokenize "if (T.length x) == 0")
