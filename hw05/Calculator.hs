module Calculator where
import ExprT

eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

-- e = Mul (Add (Lit 2) (Lit 3)) (Lit 4)
-- eval e
-- ---> 20
