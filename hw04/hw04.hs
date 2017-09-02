module HW04 ()
    where

--  | Wholemeal Programming
{- re-write the `fun1` and `fun2` in more idiomatic Haskell style.
   break each into a pipeline of incremental transformations
   to an _entire_ data strucutre. -}
fun1 :: [Integer] -> Integer
fun1 []         = 1
fun1 (x:xs)
  | even x     = (x - 2) * fun1 xs
  | otherwise  = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then (x `div` 2) else (3 * x + 1))

-- | Binary Trees
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a) -- the integer tracks height
  deriving (Show, Eq)

-- foldr, because we are building from "bottom up"
foldTree :: [a] -> Tree a
foldTree xs = foldr insert Leaf xs

insert :: a -> Tree a -> Tree a
insert value Leaf = Node 0 Leaf value Leaf
insert value (Node height left node right)
  | leftHeight > rightHeight = Node (max (leftHeight+1) rightHeight) (insert value left) node right
  | rightHeight >= leftHeight = Node (max leftHeight rightHeight+1) left node (insert value right)
    where rightHeight = treeHeight left
          leftHeight = treeHeight right

treeHeight :: Tree a -> Integer
treeHeight Leaf = 0
treeHeight (Node height _ _ _) = height


-- | More Folds
xor :: [Bool] -> Bool
xor = odd . foldr (\b accum -> if b then accum + 1 else accum) 0
-- Count how many trues there are, using an acumulator, then check if odd

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\val accum -> f val : accum) []


-- | Finding Primes
{- Best explanation is at: https://stackoverflow.com/questions/16246456/sieve-of-sundaram-list-comprehension-}
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2:[2*x+1 | x <- [1..n], x `notElem` badNums n]
-- then, from the list of ALL numbers (call them n) not in the badNums,
-- take each number, and make it odd with formula 2*n+1
-- Those are the primes, according to the sieve.

-- generate list of numbers we do NOT want
badNums :: Integer -> [Integer]
badNums n = [i+j+2*i*j|i<-[1..n],j<-[i..n]]
