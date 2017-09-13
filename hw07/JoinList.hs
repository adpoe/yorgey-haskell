{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Sized
import Data.Monoid
import Scrabble
import Buffer
import Editor

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

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s  -- make a single, with score as Monoid.
-- these can then be combined

{- Combine both kinds of annotation. A pair of Monoids is itself a Monoid. -}
type JLBuffer = JoinList (Score, Size) String

instance Buffer JLBuffer where

  -- | Convert a buffer to a String.
  -- toString :: b -> String
  toString = unlines . jlToList

  -- | Create a buffer from a String.
  -- fromString :: String -> b
  fromString = foldr (+++) Empty . map (\s -> Single (scoreString s, Size 1) s) . lines

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  -- line :: Int -> b -> Maybe String
  line = indexJ

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  -- replaceLine :: Int -> String -> b -> b
  replaceLine n rstr jlb =
   takeJ n jlb +++ Single (scoreString rstr, Size 1) rstr +++ dropJ (n+1) jlb
  -- | Compute the number of lines in the buffer.
  -- numLines :: b -> Int
  numLines = getSize . size . tag

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  -- value :: b -> Integer
  value = scorev . fst . tag
        where scorev (Score i) = i

main = runEditor editor jlb
  where jlb = fromString $ unlines xs
        xs = [ "This buffer is for notes you don't want to save, and for"
            , "evaluation of steam valve coefficients."
            , "To load a different file, type the character L followed"
            , "by the name of the file."
            ]
