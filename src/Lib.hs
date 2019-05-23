module Lib
  ( tokenize
  , toMLRInstruction
  , toMLRExpr
  )
where

import           Tokenize
import qualified Data.Text                     as T

type MLR = [Instruction]
newtype Instruction = VarAssignmentI VarAssignment deriving (Show)
data VarAssignment = VarAssignment {_varName :: T.Text, _expr :: Expr} deriving (Show)
newtype Expr = Func {_funcName :: T.Text}  deriving (Show)

toMLRInstruction :: [Token] -> Either String Instruction
toMLRInstruction (Word x : VarAssignmentT : xs) = do
  expr <- toMLRExpr xs
  Right (VarAssignmentI VarAssignment { _varName = x, _expr = expr })

toMLRExpr :: [Token] -> Either String Expr
toMLRExpr (Word x : xs) = Right Func { _funcName = x }
