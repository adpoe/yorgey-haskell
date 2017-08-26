module HW01
    (
    ) where

{- Credit Card -}
toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (div x 10) ++ [mod x 10] ++ []

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ zipWith (*) (reverse xs) (cycle [1,2])

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (flattenDigits xs)

flattenDigits :: [Integer] -> [Integer]
flattenDigits = concatMap toDigits

validate :: Integer -> Bool
validate x = rem (sumDigits $ doubleEveryOther $ toDigits x) 10 == 0

{- Towers of Hanoi -}
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi size from to spare = hanoi ((-) size 1) from spare to ++ -- move to spare, using destination as extra
                            [(from, to)] ++  -- accumulate moves
                            hanoi ((-) size 1) spare to from -- move from that spare, to the destination
