module Main where

import PullVotes
import DoProposal
import Data.Map (toList)

-- TODO make this less strict
getQuorum = do
  fmap length $ getUsers

isQuorum q = (>= q) . length . snd

main = do
  votes  <- readVoteFiles
  quorum <- getQuorum
  case filter (isQuorum quorum) (toList votes) of
    [] -> return ()
    x : _ -> doProposal x
