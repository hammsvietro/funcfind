module Main where

import Control.Exception
import Control.Monad
import Data.List (partition)
import Data.Typeable
import System.Console.ANSI
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO (hPutStrLn, stderr)
import Text.Regex.Posix

data FuncFindError
  = InvalidDirectory
  | FileNotAllowed
  | IncorrectArgs
  deriving (Typeable)

instance Show FuncFindError where
  show error = case error of
    InvalidDirectory -> "The directory provided doesn't exist."
    FileNotAllowed -> "Expected a folder as an input, but got a file."
    IncorrectArgs -> "usage:\nfuncfind [PATH] [EXPRESSION]"

instance Exception FuncFindError

exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage str e = hPutStrLn stderr str >> exitWith e

parseDirectoryFromArgs :: [String] -> IO (Maybe String)
parseDirectoryFromArgs [] = return Nothing
parseDirectoryFromArgs (arg : rest) = return (Just arg)

parseDirectory :: IO FilePath
parseDirectory = do
  result <- getArgs >>= parseDirectoryFromArgs
  currentDirectory <- getCurrentDirectory
  case result of
    Nothing -> return currentDirectory
    Just path -> return (currentDirectory </> path)

checkFileExists :: FilePath -> IO FilePath
checkFileExists path =
  doesDirectoryExist path
    >>= ( \exists ->
            if exists
              then return path
              else throwIO InvalidDirectory
        )

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p = foldM f ([], [])
 where
  f (a, b) x = do
    flag <- p x
    return $
      if flag
        then (x : a, b)
        else (a, x : b)

listDirectories :: FilePath -> IO [FilePath]
listDirectories entrypoint = do
  putStrLn entrypoint
  (files, directories) <- listDirectory entrypoint >>= partitionM doesFileExist
  foldM (\a c -> listDirectories c >>= \items -> return $ items ++ a) files directories

main :: IO ()
main =
  handle
    (\InvalidDirectory -> exitWithErrorMessage (show InvalidDirectory) (ExitFailure 1))
    $ parseDirectory
      >>= checkFileExists
      >>= listDirectories
      >>= mapM_ putStrLn
