module Main where

import           Lib

test = "thing <- 10 * 4"

main :: IO ()
main = print (tokenize test) >> print ((toMLRInstruction . tokenize) test)
