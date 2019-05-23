module Tokenize
  ( tokenize
  , Token(VarAssignmentT, Word, Function)
  )
where

import qualified Data.Text                     as T
import           Text.Read
import           Data.Char
import           SeparateSymbols

data Token = Function Function | Word T.Text | IntT  Integer |  If | VarAssignmentT | Next deriving (Show)

data Function = BuiltIn BuiltInFunc | Other T.Text deriving (Show)

data BuiltInFunc = Inline BuiltInFuncInline | Normal BuiltInFuncNormal deriving (Show)

data BuiltInFuncNormal = Input | Output deriving (Show)
data BuiltInFuncInline = Add | Subtract | Multiply | Divide | Or | And | Not  deriving (Show)

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
  Just a  -> IntT a
  Nothing -> case x of
    "+"      -> Function $ BuiltIn $ Inline Add
    "-"      -> Function $ BuiltIn $ Inline Subtract
    "*"      -> Function $ BuiltIn $ Inline Multiply
    "/"      -> Function $ BuiltIn $ Inline Divide
    "OR"     -> Function $ BuiltIn $ Inline Or
    "AND"    -> Function $ BuiltIn $ Inline And
    "NOT"    -> Function $ BuiltIn $ Inline Not
    "INPUT"  -> Function $ BuiltIn $ Normal Input
    "OUTPUT" -> Function $ BuiltIn $ Normal Output
    _        -> if isUpper $ T.head x then Function $ Other x else Word x
