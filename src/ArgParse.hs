module ArgParse (readPathArg, checkFilePath) where

import System.Exit ( exitFailure, exitSuccess )
import System.Environment ( getArgs, getExecutablePath )
import System.FilePath ((</>), takeDirectory)
import System.Directory (doesFileExist, makeAbsolute)
import Data.Functor ( (<&>) )
import Control.Monad ( unless )


readPathArg :: IO FilePath
readPathArg = getArgs >>= parse
  where
    parse :: [String] -> IO FilePath
    parse ["-h"] = usage >> exit
    parse ["-v"] = version >> exit
    parse [] = usage >> exit
    parse [x] = makeAbsolute x
    parse _ = usage >> exit
    version = putStrLn "movMidi version 0.0"
    usage = putStrLn "Usage: movMidi [-v|-h|CONFIG_PATH]"
    exit = exitSuccess

checkFilePath :: String -> FilePath -> IO ()
checkFilePath typ path = do
  putStrLn $ "Specified " ++ typ ++ " file path: " ++ path
  exist <- doesFileExist path
  unless exist $ putStrLn "  is not found..." >> exitFailure