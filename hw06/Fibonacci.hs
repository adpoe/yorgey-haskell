module Fibonacci
    (
    ) where

fib :: Integer -> Integer
fib n
  | n <= 1    = 1
  | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]
