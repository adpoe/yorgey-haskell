{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree
import Data.List

-- add an employee to GuestList, updating cached fun score appropriately
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp name empfun) (GL es totalfun) = GL (e : es) (totalfun + empfun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend gl1@(GL es1 totalfun1) gl2@(GL es2 totalfun2) = GL (es1 ++ es2) (totalfun1 + totalfun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2
  | gl1 > gl2 = gl1
  | otherwise = gl2


-- implement a tree fold for Data.Tree
-- key point:  we are applying f to the rootLabel
--             but need to map/fold the function over the whole subForest list
--             ... so essentially need to pull inner value out and keep mapping
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node {rootLabel = a, subForest = xs} = f a (map (treeFold f) xs)

-- okay, so mconcat can reduce a monoid to a single value..
{-Used implementation from: https://github.com/fate-lovely/cis194/blob/master/08-IO/Party.hs-}

-- Clarifying thoughts:
-- this works because glCons creates the GuestList from scratch, using Monoid, if necessary.
-- and the way that treeFold is defined, it just needs a function of propert type
-- and any tree of `a`'s --> and it can produce the desired output tuple
-- doesn't matter that we aren't passing it a (GL, GL) tuple that was created
-- a priori. It gets made in process.

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = let
  withBoss = glCons boss (mconcat $ buildResults snd)
  withoutBoss = mconcat $ buildResults fst
  buildResults getter = map getter results
  in (withBoss, withoutBoss)

betterResult :: (GuestList, GuestList) -> GuestList
betterResult (withBoss, withoutBoss)
  | withBoss > withoutBoss = withBoss
  | otherwise              = withoutBoss

maxFun :: Tree Employee -> GuestList
maxFun tree = betterResult $ treeFold nextLevel tree

formatGL :: GuestList -> String
formatGL (GL emps fun) =
  let firstLine = "Total fun: " ++ show fun
      names = sort $ map empName emps  -- can map a record syntax getter over record
  in unlines $ firstLine : names

main :: IO ()
main = readFile "./company.txt" >>= putStrLn . formatGL . maxFun . read
-- read the file and put the data computation into this pipeline
-- starting from right
