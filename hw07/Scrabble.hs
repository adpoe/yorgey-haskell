module Scrabble where

{- Cache scrabble score for each line in the buffer. -}


data Score = Score Int
             deriving (Eq, Show)

instance Num Score where
  (Score a) + (Score b) = Score (a + b)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | c `elem` "aeilnorstuAEILNORSTU" = Score 1
  | c `elem` "dgDG"                 = Score 2
  | c `elem` "bcmpBCMP"             = Score 3
  | c `elem` "fhvwyFHVWY"           = Score 4
  | c `elem` "kK"                   = Score 5
  | c `elem` "jxJX"                 = Score 8
  | c `elem` "qzQZ"                 = Score 10
  | otherwise                       = Score 0

scoreString :: String -> Score
scoreString = mconcat . map score
