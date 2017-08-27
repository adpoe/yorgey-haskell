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
