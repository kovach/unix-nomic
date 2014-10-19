module Main where

import System.Posix.User
import System.FilePath.Posix
import System.Directory
import System.Environment
import Data.Maybe

numPrefix :: String -> Maybe Int
numPrefix str = 
  case reads str of
    [] -> Nothing
    x : _ -> Just $ fst x

moveProp :: FilePath -> Int -> IO ()
moveProp name number = do
  copyFile name $ proposalDir ++ "/" ++ show number ++ "_" ++ snd (splitFileName name)

proposalDir = "/nomic/proposals"

getProposals = getDirectoryContents proposalDir

newPropID = (1+) . maximum . (0:) . mapMaybe numPrefix

main = do
  args <- getArgs
  case args of
    [] -> do putStrLn "must specify a proposal!"
    p : _ -> do
      getEffectiveUserID >>= setUserID
      entries <- getProposals
      let new_id = newPropID entries
      moveProp p new_id
