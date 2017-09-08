module Fibonacci
    (
    ) where

fib :: Integer -> Integer
fib n
  | n <= 0    = 0
  | n == 1    = 1
  | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- https://wiki.haskell.org/The_Fibonacci_sequence
-- The "Canonical zipWith implementation"
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)
