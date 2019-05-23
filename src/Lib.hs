module Lib
  ( tokenize
  , toMLRInstruction
  )
where

import           Tokenize
import qualified Data.Text                     as T

type MLR = [Instruction]
data Instruction = VarAssignmentI VarAssignment deriving (Show)
data VarAssignment = VarAssignment {_varName :: T.Text, _expr :: Expr} deriving (Show)
data Expr = Expr deriving (Show)

toMLRInstruction :: [Token] -> Either String Instruction
toMLRInstruction (Word x : VarAssignmentT : xs) = do
  expr <- toMLRExpr xs
  Right (VarAssignmentI (VarAssignment { _varName = x, _expr = expr }))

toMLRExpr :: [Token] -> Either String Expr
toMLRExpr x = Right Expr
