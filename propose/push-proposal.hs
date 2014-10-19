module Main where

import System.Posix.User
import System.Posix.Types (UserID)
import System.FilePath.Posix
import System.Directory
import System.Environment
import System.Exit
import Data.Maybe

proposalDir = "/nomic/proposals"

numPrefix :: String -> Maybe Int
numPrefix str = 
  case reads str of
    [] -> Nothing
    x : _ -> Just $ fst x

moveProp :: FilePath -> Int -> UserID -> IO ()
moveProp name number uid = do
  copyFile name $ proposalDir ++ "/" ++ show number ++ "_" ++ show uid ++ "_" ++ snd (splitFileName name)

getProposals = getDirectoryContents proposalDir

newPropID = (1+) . maximum . (0:) . mapMaybe numPrefix

main = do
  args <- getArgs
  case args of
    [] -> do putStrLn "Usage: push-proposal proposal.tar.gz"
             exitWith $ ExitFailure 128
    p : _ -> do
      uid <- getRealUserID
      getEffectiveUserID >>= setUserID
      entries <- getProposals
      let new_id = newPropID entries
      moveProp p new_id uid
