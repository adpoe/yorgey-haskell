module Calculator where

import ExprT
import Parser

eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- e = Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-- eval e
-- ---> 20
evalStr :: String -> Maybe Integer
evalStr s = maybeEval $ parseExp ExprT.Lit ExprT.Add ExprT.Mul s

maybeEval :: Maybe ExprT -> Maybe Integer
maybeEval (Just e) = Just (eval e)
maybeEval Nothing = Nothing
