module Tokenize
  ( tokenize
  )
where

import qualified Data.Text               as T
import           SeparateSymbols

data Token = Word T.Text | LeftBracket | RightBracket | If deriving (Show)

tokenize :: T.Text -> [Token]
tokenize = tokenizeSymbols . separateSymbols

tokenizeSymbols :: [T.Text] -> [Token]
tokenizeSymbols = map $
  \x -> case x of
    "("  -> LeftBracket
    ")"  -> RightBracket
    "if" -> If
    _    -> Word x
  
