{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = parseMessage' (words s)

parseMessage' :: [String] -> LogMessage
parseMessage' ("I":t:xs) = LogMessage Info (read t) (unwords xs)
parseMessage' ("W":t:xs) = LogMessage Warning (read t) (unwords xs)
parseMessage' ("E":e:t:xs) = LogMessage (Error (read e)) (read t) (unwords xs)
parseMessage' (_:xs) = Unknown (unwords xs) -- If we don't have a match, it's just info.
parseMessage' [] = Unknown ""

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)
