module PullVotes where

import System.Posix.User
import System.FilePath.Posix
import System.Directory
import System.Environment
import System.Process
import System.IO.Error
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Char
import qualified Data.Map as M

voteFileName name = "/home/" ++ name ++ "/votes"

ignoreDots = mapMaybe catchDot
 where
   catchDot ('.' : _) = Nothing
   catchDot x = Just x

safeRead user = do
  e <- tryJust (guard . isDoesNotExistError) (readFile (voteFileName user))
  let str = either (const "foo") id e
  return str

ignoreHashes = mapMaybe catchHash
 where
   catchHash ('#' : _) = Nothing
   catchHash x = Just x
parseNum :: String -> Maybe (Int, String)
parseNum str =
  case reads str of
    [] -> Nothing
    x : _ -> Just x
parseChar :: String -> Maybe (Char, String)
parseChar str =
  case dropWhile isSpace str of
    [] -> Nothing
    x : xs -> Just (x, xs)
parseLine str = do
  (prop, str') <- parseNum str
  (vote, _) <- parseChar str'
  let bool = case vote of
              'y' -> True
              _   -> False
  return (prop, bool)
parseVoteFile :: String -> [(Prop, Bool)]
parseVoteFile str =
  let stripped = ignoreHashes . lines $ str
  in mapMaybe parseLine stripped

type User = String
type Prop = Int

foldUserVotes :: (User, [(Prop, Bool)]) -> M.Map Prop [User] -> M.Map Prop [User]
foldUserVotes (user, votes) voteMap =
  foldr (\(prop, bool) m -> if bool then M.insertWith (++) prop [user] m else m)
        voteMap
        votes

foldVotes :: [(User, [(Prop, Bool)])] -> M.Map Prop [User]
foldVotes userVotes = foldr foldUserVotes M.empty $ userVotes

getUsers :: IO [User]
getUsers = do
  (code, stdout, stderr) <-
    readProcessWithExitCode "members" ["players"] ""
  return (words stdout)

readUserFiles = do
  users <- getUsers
  files <- mapM safeRead users
  return $ zip users files

mapSnd f (a, b) = (a, f b)

readVoteFiles = do
  pairs <- readUserFiles
  let voteSets = foldVotes (map (mapSnd parseVoteFile) pairs)
  return voteSets
