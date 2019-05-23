module Main where

import           Lib

test = "10 * 4"

main :: IO ()
main = do
    print $ tokenize test
    print $ (toMLRInstruction . tokenize) test
    print $ (toMLRExpr . tokenize) test
