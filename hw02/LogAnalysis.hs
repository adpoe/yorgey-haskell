{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

{- OUTPUT
ghci> testWhatWentWrong parse whatWentWrong "error.log"

["Mustardwatch opened, please close for proper functioning!",
"All backup mustardwatches are busy",
"Depletion of mustard stores detected!",
"Hard drive failure: insufficient mustard",
"All backup mustardwatches are busy",
"Twenty seconds remaining until out-of-mustard condition",
"Ten seconds remaining until out-of-mustard condition",
"Empty mustard reservoir! Attempting to recover...",
"Recovery failed! Initiating shutdown sequence"]

-}

import Log

parseMessage :: String -> LogMessage
parseMessage s = parseMessage' (words s)

parseMessage' :: [String] -> LogMessage
parseMessage' ("I":t:xs) = LogMessage Info (read t) (unwords xs)
parseMessage' ("W":t:xs) = LogMessage Warning (read t) (unwords xs)
parseMessage' ("E":e:t:xs) = LogMessage (Error (read e)) (read t) (unwords xs)
parseMessage' (_:xs) = Unknown (unwords xs)
parseMessage' [] = Unknown ""

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert target@(LogMessage _ t1 _) tree@(Node left logmsg@(LogMessage _ t2 _) right)
  | t1 > t2 = Node left logmsg (insert target right)
  | t2 <= t2 = Node (insert target left) logmsg right
insert _ _ = Leaf

build :: [LogMessage] -> MessageTree
build msgs = foldr insert Leaf msgs -- accumulate the list of mgs in a leaf,
                                    -- using insert as the accumulation fn

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map getLogMsg (filterErrors msgs)

getLogMsg :: LogMessage -> String
getLogMsg (LogMessage _ _ s) = s
getLogMsg (Unknown s) = s

filterErrors :: [LogMessage] -> [LogMessage]
filterErrors msgs = filter seriousErrors sortedMsgs
  where sortedMsgs = inOrder (build msgs)

seriousErrors :: LogMessage -> Bool
seriousErrors (LogMessage (Error sev) _ _) = sev >= 50
seriousErrors _ = False
