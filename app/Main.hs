module Main where

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO (hPutStrLn, stderr)

type FileVerification = Either String FilePath

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

checkFileExists :: FilePath -> IO FileVerification
checkFileExists path =
  doesDirectoryExist path
    >>= ( \exists ->
            if exists
              then return (Right path)
              else return (Left "Direcotry doesn't exist.")
        )

handleFileVerification :: FileVerification -> IO FilePath
handleFileVerification (Left err) = exitWithErrorMessage err $ ExitFailure $ -1
handleFileVerification (Right path) = return path

main :: IO ()
main =
  parseDirectory
    >>= checkFileExists
    >>= handleFileVerification
    >>= listDirectory
    >>= mapM_ putStrLn
