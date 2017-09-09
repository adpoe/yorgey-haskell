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

data Stream a = Cons' a (Stream a)

-- can't show infinite Stream
-- so make into a list, and just call the List show on first 20
instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons' x xs) = x : streamToList xs
-- use your custom constructor to pattern match this and just put into a list

streamRepeat :: a -> Stream a
streamRepeat x = Cons' x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons' x xs) = Cons' (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons' seed (streamFromSeed f (f seed))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons' x xs) s = Cons' x (interleaveStreams s xs) -- alternate

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler  :: Stream Integer
ruler = streamMap f (streamFromSeed (+1) 1)
  where f x | odd x = 0
            | otherwise = 1 + f (x `div` 2)
