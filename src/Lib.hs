module Lib
    ( 
     tokenizeWord
    )
where

import qualified Data.Text.Lazy             as T



data WordWithEnd =  WordWithEnd {word::T.Text, endSymbol:: T.Text }


tokenizeWord :: T.Text -> [T.Text]
tokenizeWord x =   moveOut  "==" "if (T.length x) == 0"

addInside :: T.Text -> [T.Text] -> [T.Text]
addInside with l@(x:xs) = x:(if length l /= 1 then (with:(addInside with xs))else addInside with xs)
addInside with [] = []

moveOut x t =  ( addInside x . T.splitOn x) t