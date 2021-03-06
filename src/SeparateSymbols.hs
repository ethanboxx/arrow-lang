module SeparateSymbols
  ( separateSymbols
  )
where

import qualified Data.Text                     as T

separateSymbols :: T.Text -> [T.Text]
separateSymbols x =
  filter (/= "")
    $ filter (/= " ")
    $ moveOut " "
    $ moveOut "<-"
    $ moveOut "if"
    $ moveOut "\n"
    $ moveOut ";" [x]

addInside :: T.Text -> [T.Text] -> [T.Text]
addInside with l@(x : xs) =
  x : if length l /= 1 then with : addInside with xs else addInside with xs
addInside _ [] = []

moveOut :: T.Text -> [T.Text] -> [T.Text]
moveOut x = concatMap $ addInside x . T.splitOn x
