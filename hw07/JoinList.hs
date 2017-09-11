module JoinList where

import Sized
import Data.Monoid

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2
-- combine just the `m` values in top 'node' and append lists below
-- ... when you pass around monoids, you can perform monoid operations on them
-- ... de-structuring via pattern match, as shown above. interesting.
-- ... Monoid as an interface, with functions & context that we pass around

-- grab monoid annotation from root of list, no matter its form
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m


-- | Fast Indexing -- cache the size of each subtree.
--    use at each step to determine if desired index is L or R branch
--    Find the JoinList element at the specified index. If OOB --> Nothing.
indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ i (Append m l r)
  | i < 0 = Nothing
  | i >= getSize (size m) = Nothing
  | i >= leftSize = indexJ (i - leftSize) r  -- subtract leftSize because that much of list is removed
  | otherwise = indexJ i l
  where leftSize = getSize . size . tag $ l -- grab left monoid, and find its size

-- | Safe List Index Function
(!!?) :: [a] -> Int -> Maybe a
[] !!? _          = Nothing
_  !!? i | i < 0  = Nothing
(x:xs) !!? 0      = Just x  -- Keep recursing until we hit 0, dropping `i` each time
(x:xs) !!? i      = xs !!? (i-1)

-- | Convert 2 Join Lists into Lists
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a)  = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- | Drop the first N elements of a JoinList
dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i _ | i < 0 = Empty
dropJ 0 jl = jl
dropJ i (Single _ _) | i > 1 = Empty
dropJ i (Append m l r)
  | i > getSize (size m) = Empty
  | i < leftSize = dropJ i l +++ r  -- keep right, and drop from left
  | i >= leftSize = dropJ (i - leftSize) r
  where leftSize = getSize . size . tag $ l

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i _ | i <= 0 = Empty
takeJ _ jl@(Single _ _) = jl
takeJ i jl@(Append m l r)
  | i > getSize (size m) = jl
  | i < leftSize = takeJ i l -- keep examining left
  | i >= leftSize = l +++ takeJ (i - leftSize) r -- build up. use all of left and what's needed in right
  where leftSize = getSize . size . tag $ l
