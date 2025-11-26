{-# LANGUAGE TupleSections #-}

module ArgParse (readPathArg, checkFilePath) where

import System.Exit ( exitFailure, exitSuccess )
import System.Environment (getArgs)
import System.Directory (doesFileExist, makeAbsolute)
import Control.Monad ( unless )


readPathArg :: IO (Int, FilePath)
readPathArg = getArgs >>= parse
  where
    parse :: [String] -> IO (Int, FilePath)
    parse ["-h"] = usage >> exit
    parse ["-v"] = version >> exit
    parse [] = usage >> exit
    parse [midiInId, path] = (read midiInId,) <$> makeAbsolute path
    parse _ = usage >> exit
    version = putStrLn "movMidi version 0.0"
    usage = putStrLn "Usage: movMidi [-v|-h|MIDI_IN_ID CONFIG_PATH]"
    exit = exitSuccess

checkFilePath :: String -> FilePath -> IO ()
checkFilePath typ path = do
  putStrLn $ "Specified " ++ typ ++ " file path: " ++ path
  exist <- doesFileExist path
  unless exist $ putStrLn "  is not found..." >> exitFailure