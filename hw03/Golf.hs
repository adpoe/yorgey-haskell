module Golf where
{-# LANGUAGE FlexibleInstances #-}

import Data.List

-- Hopscotch
skips :: [a] -> [[a]]
skips xs = skips' xs (length xs)

getNths :: Int -> [a] -> [Int]
getNths nth xs = map (subtract 1) $ takeWhile (<= length xs) $ map (nth*) (enumFromTo 1 (length xs))

skips' :: [a] -> Int -> [[a]]
skips' [] _ = []
skips' _ 0  = []
skips' xs n = skips' xs (subtract 1 n) ++ [accum]
  where accum = map (xs !!) (getNths n xs)
{- Opted for readability (sorta) instead of pure code golfing -}


-- Local Maxima
localMaxima :: [Integer] -> [Integer]
localMaxima xs = map getMax $ filter isMaxima $ map cleanCombo (combos xs)

-- point-free version
localMaxima' :: [Integer] -> [Integer]
localMaxima' = map getMax . filter isMaxima . map cleanCombo . combos

combos :: [Integer] -> [((Integer, Integer), Integer)]
combos xs = zip (zip xs (tail xs)) (tail (tail xs))

cleanCombo :: ((Integer, Integer), Integer) -> (Integer, Integer, Integer)
cleanCombo ((a, b), c) = (a, b, c)

cleanCombos :: [((Integer, Integer), Integer)] -> [(Integer, Integer, Integer)]
cleanCombos = map cleanCombo

isMaxima :: (Integer, Integer, Integer) -> Bool
isMaxima (a,b,c) = (b > a) && (b > c)

getMax :: (Integer, Integer, Integer) ->Integer
getMax (_,b,_) = b

-- Histogram
{- from: https://github.com/bschwb/cis194-solutions/blob/master/03-rec-poly/Golf.hs

  Tried.. and read a few solutons, but this is the most concise I've seen.
  I don't take cred it for it. But very nice algorithm.

  What I initially had trouble with was mutating state. Here, we don't do that.
  We create containing [(maxCount) ... 1], and for all frequencies,
  if the the frequency at that number >= line number, then fill that line with '*'.

  Also keps the data data structures to 2 lists, where the indices act as an implicit numbering system,
  for both frequency counts, and line number.

  Hadn't seen mapping a list comprehension over another list before, either. Nice touch.
-}
histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

count :: [Integer] -> [Int]
count xs = map (\n -> length $ filter (== n) xs) [0..9]
