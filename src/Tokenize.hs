module Tokenize
  ( tokenize
  )
where

import qualified Data.Text               as T
import           SeparateSymbols

data Token = Word T.Text | LeftBracket | RightBracket | If | ArrowRight | ArrowLeft deriving (Show)

tokenize :: T.Text -> [Token]
tokenize = tokenizeSymbols . separateSymbols

tokenizeSymbols :: [T.Text] -> [Token]
tokenizeSymbols = map $
  \x -> case x of
    "("  -> LeftBracket
    ")"  -> RightBracket
    "if" -> If
    "<" -> ArrowLeft
    ">" -> ArrowRight
    _    -> Word x
  
