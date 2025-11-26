{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Sound.PortMidi (PMEvent(..), DeviceInfo(..),
                       initialize, countDevices, getDeviceInfo,
                       openInput, readEvents, setFilter,
                       filterSysex, decodeMsg, status, data1, data2,
                       input, PMStream)
import Control.Monad (forever, foldM)
import Data.Bits ((.&.))
import Data.List (nub)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8  as BS
import System.IO
import Network.Socket
        (Socket, SockAddr(SockAddrUnix), socket, connect
        , Family(AF_UNIX), SocketType(Stream), socketToHandle)
import System.Directory (doesFileExist)
import System.Posix.Types (Fd(..))
import Control.Concurrent (threadDelay, forkIO, ThreadId, killThread)
import Control.Exception (catch, IOException)
import qualified Dhall as DHL

import ArgParse (readPathArg, checkFilePath)

data Config = Config { cfgChannel :: DHL.Natural
                     , cfgNote :: DHL.Natural
                     , cfgFrom :: Double
                     , cfgTo :: Double
                     , cfgSocket :: DHL.Text
                     }
    deriving (DHL.Generic, Show)

instance DHL.FromDhall Config

type NoteMap = M.Map (Int, Int) [(FilePath, Double, Double)]
type ThreadMap = M.Map FilePath (Maybe ThreadId)
type SockMap = M.Map FilePath Handle

enumerateDevices :: IO [DeviceInfo]
enumerateDevices = do
  count <- countDevices
  mapM getDeviceInfo [0 .. count - 1]

waitForSocket :: FilePath -> IO ()
waitForSocket path = do
  exists <- doesFileExist path
  if exists then pure ()
            else do
              threadDelay 200000
              waitForSocket path

connectToMPV' :: FilePath -> IO Handle
connectToMPV' path = do
  sock <- socket AF_UNIX Stream 0 :: IO Socket
  connect sock (SockAddrUnix path)
  socketToHandle sock WriteMode

connectToMPV :: FilePath -> IO Handle
connectToMPV path = do
  sock <- socket AF_UNIX Stream 0 :: IO Socket
  connect sock (SockAddrUnix path)
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h LineBuffering

  -- Reader thread: consume mpv replies so its send buffer doesn't fill
  _ <- forkIO $ forever $ do
         -- mpv sends JSON lines; if mpv quits, this will throw
         BS.hGetLine h `catch` \(_ :: IOException) -> pure BS.empty
  return h

sendMPV' :: Handle -> String -> IO ()
sendMPV' h msg = do
    BS.hPutStrLn h $ BS.pack msg
    hFlush h
    return ()

sendMPV :: Handle -> String -> IO ()
sendMPV h msg =
  (BS.hPutStrLn h (BS.pack msg) >> hFlush h)
    `catch` \e -> do
      let _ = e :: IOException
      putStrLn $ "mpv write failed: " ++ show e

main :: IO ()
main = do
  -- Read config path from command line.
  (midiInId, cfgPath) <- readPathArg
  checkFilePath "config" cfgPath
  -- Load config.
  configs <- DHL.inputFile DHL.auto cfgPath :: IO [Config]
  print configs

  let sockPaths = nub $ map (T.unpack . cfgSocket) configs :: [FilePath]

  -- Connect to mpv socket
  putStrLn "Waiting for mpv socket..."
  mapM_ waitForSocket sockPaths 

  putStrLn "Connecting to mpv..."
  sockMap <- foldM (\m path -> flip (M.insert path) m <$> connectToMPV path)
                   M.empty sockPaths :: IO SockMap
  putStrLn "Connected to mpv."

  -- Convert configs to NoteMap.
  let noteMap = foldl (\m c -> M.insertWith (++)
                                 (fromEnum $ cfgChannel c, fromEnum $ cfgNote c)
                                 [(T.unpack $ cfgSocket c, cfgFrom c , cfgTo c)]
                                 m)
                  M.empty configs :: NoteMap

  -- Initialize PortMIDI
  initialize

  -- List devices
  devs <- enumerateDevices
  mapM_ (\(id, info) -> putStrLn $ show id ++ ": " ++ show (name info))
    . filter (input . snd) $ zip [0..] devs

  let devID = midiInId

  r <- openInput devID
  case r of
    Left err -> putStrLn ("Error opening MIDI: " ++ show err)
    Right pmStream -> do
      putStrLn "Listening for MIDI..."
      setFilter pmStream filterSysex
      let
        hEventFunc = handleEvent noteMap sockMap 
        threadMap = M.fromList . map (, Nothing) $ M.keys sockMap  :: ThreadMap
      playLoop threadMap hEventFunc pmStream

playLoop :: ThreadMap
         -> (ThreadMap -> PMEvent -> IO ThreadMap)
         -> PMStream -> IO ()
playLoop threadMap hEventFunc pmStream = do
  evs <- readEvents pmStream
  case evs of
    Right events -> do
      threadMap' <- foldM hEventFunc threadMap events
      playLoop threadMap' hEventFunc pmStream
    Left _       -> pure ()

playAt:: Handle -> Double -> IO ()
playAt h time = do
  sendMPV h ("{\"command\": [\"seek\", \"" ++ show time ++ "\", \"absolute\"] }")
  sendMPV h "{\"command\": [\"set_property\", \"pause\", false] }"
  return ()

pause:: Handle -> IO ()
pause = flip sendMPV "{\"command\": [\"set_property\", \"pause\", true]}"

genPlayThread :: SockMap -> ThreadMap
              -> (String, Double, Double)
              -> IO ThreadMap
genPlayThread sockMap threadMap (sockPath, from, to) = case threadMap M.! sockPath of
  Just tid -> do
    -- Kill existing thread if any.
    putStrLn "Killing existing thread to resume."
    killThread tid
    let threadMap' = M.insert sockPath Nothing threadMap
    genPlayThread sockMap threadMap' (sockPath, from, to)
  Nothing -> do
    putStrLn "Starting new play thread."
    -- Start new thread.
    tid <- forkIO $ do
              playAt (sockMap M.! sockPath) from
              let waitTime = round ((to - from) * 1e6) :: Int
              threadDelay waitTime
              pause $ sockMap M.! sockPath
    let threadMap' = M.insert sockPath (Just tid) threadMap
    return threadMap'

cancelThread :: SockMap -> ThreadMap  -> FilePath -> IO ThreadMap
cancelThread sockMap threadMap sockPath = case threadMap M.! sockPath of
  Just tid -> do
    putStrLn "Killing existing thread as cancel."
    killThread tid
    pause $ sockMap M.! sockPath
    let threadMap' = M.insert sockPath Nothing threadMap
    return threadMap'
  Nothing -> return threadMap

handleEvent :: NoteMap -> SockMap -> ThreadMap  -> PMEvent -> IO ThreadMap
handleEvent noteMap sockMap threadMap (PMEvent clMsg _) =
  let 
      msg = decodeMsg clMsg
      stat = status msg
      d1 = fromEnum $ data1 msg :: Int
      d2 = fromEnum $ data2 msg :: Int
      cmd = stat .&. 0xF0
      chan = fromEnum $ stat .&. 0x0F :: Int
      hitNotes = M.lookup (chan, d1) noteMap  :: Maybe [(FilePath, Double, Double)]
      cancel = case hitNotes of
                Just xs -> foldM
                            (\tm (sockPath, _, _) -> cancelThread sockMap tm sockPath)
                            threadMap xs :: IO ThreadMap
                Nothing -> return threadMap
   in case cmd of
        0x90 -> if d2 == 0
                  then do
                    putStrLn $ "chan:" ++ show chan
                                ++ " Note Off (via velocity 0): "
                                ++ show d1
                    -- cancel
                    return threadMap
                  else do
                    putStrLn $ "chan:" ++ show chan ++ " Note On: " ++ show d1
                    case hitNotes of
                      Nothing -> return threadMap
                      Just xs ->
                        foldM (genPlayThread sockMap) threadMap xs
        0x80 -> do
                  putStrLn $ "chan:" ++ show chan ++ " Note Off: " ++ show d1
                  -- cancel
                  return threadMap
        _    -> return threadMap
