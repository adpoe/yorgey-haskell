module Golf where

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
