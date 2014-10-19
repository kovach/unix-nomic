module Main where

import System.Posix.Temp
import System.FilePath.Posix
import System.Process
import System.Environment
import System.Exit
import Data.Maybe
import System.Console.GetOpt
import System.Directory
import Control.Monad
import Control.Applicative
import System.IO

data ProposeOpts
  = ProposeOpts { filename :: Maybe String
                , helpMode :: Bool
                }

defaultOptions  = ProposeOpts { filename = Nothing
                              , helpMode = False }

options :: [OptDescr (ProposeOpts -> ProposeOpts)]
options =
  [
    Option ['h'] ["help"] (NoArg help') "Prints this usage information"
  ]
  where help' opts = opts { helpMode = True }


parseOpts :: [String] -> IO ProposeOpts
parseOpts argv
  = case getOpt argorder options argv of
      (o,_,[]) -> let opts = foldl (flip id) defaultOptions o
                  in case helpMode opts  of
                    True -> do putStr $ usageInfo header options
                               exitSuccess
                    False -> case filename opts of
                      Just _ -> return opts
                      Nothing -> do putStr $ usageInfo header options
                                    exitWith $ ExitFailure 1
      (_,_,errs) -> do putStr (concat errs ++ usageInfo header options)
                       exitWith $ ExitFailure 1

    where header = "Usage: propose [OPTIONS...] proposal"
          argorder = ReturnInOrder (\s opts -> opts { filename = Just s })

checkIfTarGz :: FilePath -> IO (Maybe [String])
checkIfTarGz path = do
  (code, stdout, stderr) <- readProcessWithExitCode "tar" ["-tzf", path] ""
  case code of
    ExitSuccess -> return . Just $ lines stdout
    ExitFailure _ -> return Nothing

packageScript :: FilePath -> IO ()
packageScript fn = do
  tmpdir <- getTemporaryDirectory >>= mkdtemp
  let propname = snd $ splitFileName $ dropExtensions fn
  let newdir = combine tmpdir propname
  createDirectory newdir
  runShellCommand "cp" [fn, combine newdir "run.sh"]
  let newfn = newdir ++ ".tar.gz"
  runShellCommand "tar" ["-czf", newfn, "-C", newdir, "."]
  print newfn
  filelist <- checkIfTarGz newfn
  case filelist of
    Just list -> propose newfn list
    Nothing -> do putStrLn "Something went wrong making the tar.gz"
                  exitWith $ ExitFailure 1

packageDirectory :: FilePath -> IO ()
packageDirectory fn = do
  tmpdir <- getTemporaryDirectory >>= mkdtemp
  let newdir = combine tmpdir (snd $ splitFileName fn)
  runShellCommand "cp" ["-R", fn, newdir]
  let newfn = newdir ++ ".tar.gz"
  runShellCommand "tar" ["-czf", newfn, "-C", newdir, "."]
  print newfn
  filelist <- checkIfTarGz newfn
  case filelist of
    Just list -> propose newfn list
    Nothing -> do putStrLn "Something went wrong making the tar.gz"
                  exitWith $ ExitFailure 1

runShellCommand :: FilePath -> [String] -> IO ()
runShellCommand path args = do
  (code, stdout, stderr) <- readProcessWithExitCode path args ""
  case code of
    ExitSuccess -> return ()
    ExitFailure _ -> do putStr stdout
                        putStr stderr
                        exitWith code

propose :: FilePath -> [String] -> IO ()
propose fn files = do
  when (not $ "./run.sh" `elem` files) $ do
    putStr "Missing run.sh inside proposal.  Propose anyway? [y/N] "
    hFlush stdout
    ans <- getLine
    when (ans /= "y") $ exitWith $ ExitFailure 1
  (code, stdout, stderr) <- readProcessWithExitCode "push-proposal" [fn] ""
  case code of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      putStrLn "Failure pushing proposal."
      putStr stderr
      exitWith code

main = do
  opts <- getArgs >>= parseOpts
  fn <- canonicalizePath $ fromJust $ filename opts
  fexist <- doesFileExist fn
  dexist <- doesDirectoryExist fn
  if fexist
    then do tz <- checkIfTarGz fn
            case tz of
              Nothing -> packageScript fn
              Just files -> propose fn files
    else if dexist
         then packageDirectory fn
         else do putStrLn "No such proposal file or directory"
                 exitWith $ ExitFailure 1


--  path <- mkdtemp
