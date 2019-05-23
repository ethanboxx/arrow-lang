module Tokenize
  ( tokenize
  , Token(VarAssignmentT, Word)
  )
where

import qualified Data.Text                     as T
import           Text.Read                    
import           SeparateSymbols

data Token = Word T.Text | IntT  Integer |  If | VarAssignmentT | Next deriving (Show)

data BuiltInFunc = Inline | Normal

data BuiltInFuncNormal = Input | Output
data BuiltInFuncInline = Add | Multiply | Divide | Subtract

tokenize :: T.Text -> [Token]
tokenize = tokenizeSymbols . separateSymbols

tokenizeSymbols :: [T.Text] -> [Token]
tokenizeSymbols = map tokenizeSymbol


tokenizeSymbol :: T.Text -> Token
tokenizeSymbol "if" = If
tokenizeSymbol "<-" = VarAssignmentT
tokenizeSymbol "\n" = Next
tokenizeSymbol ";"  = Next
tokenizeSymbol x    = case (readMaybe . T.unpack) x of
  Just x  -> IntT x
  Nothing -> Word x