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

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-- Additional Instances of the type classes
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit a = a > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7  where
  lit a = Mod7 (mod a 7)
  add (Mod7 a) (Mod7 b) = Mod7 (mod (a + b) 7)
  mul (Mod7 a) (Mod7 b) = Mod7 (mod (a * b) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
