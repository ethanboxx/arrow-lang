module Lib
    ( 
  separateSymbols,
     moveOut
    
    )
where

import qualified Data.Text.Lazy               as T



data WordWithEnd =  WordWithEnd {word::T.Text, endSymbol:: T.Text }


separateSymbols :: T.Text -> [T.Text]
separateSymbols x =   (filter (/= "") . filter (/= " ") . moveOut  "(" . moveOut  ")" . moveOut  " " . moveOut  "if" . moveOut  "==") ["if (T.length x) == 0"]

addInside :: T.Text -> [T.Text] -> [T.Text]
addInside with l@(x:xs) = x:(if length l /= 1 then (with:(addInside with xs))else addInside with xs)
addInside with [] = []

moveOut:: T.Text -> [T.Text] -> [T.Text]
moveOut x t = concat (map ( addInside x . T.splitOn x) t)