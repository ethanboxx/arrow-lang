module Tokenize
  ( tokenize
  )
where

import qualified Data.Text                     as T
import           SeparateSymbols

data Token = Word T.Text | LeftBracket | RightBracket | If | ArrowRight | ArrowLeft | Next deriving (Show)

tokenize :: T.Text -> [Token]
tokenize = tokenizeSymbols . separateSymbols

tokenizeSymbols :: [T.Text] -> [Token]
tokenizeSymbols = map tokenizeSymbol


tokenizeSymbol :: T.Text -> Token
tokenizeSymbol "("  = LeftBracket
tokenizeSymbol ")"  = RightBracket
tokenizeSymbol "if" = If
tokenizeSymbol "<"  = ArrowLeft
tokenizeSymbol ">"  = ArrowRight
tokenizeSymbol "\n" = Next
tokenizeSymbol ";"  = Next
tokenizeSymbol x    = Word x

