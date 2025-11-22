{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import Sound.PortMidi (PMEvent(..), DeviceInfo(..),
                       initialize, countDevices, getDeviceInfo,
                       openInput, readEvents, setFilter,
                       filterSysex, decodeMsg, status, data1, data2,
                       input)
import Control.Monad (forever)
import Data.Bits ((.&.))
import qualified Data.ByteString.Char8  as BS
import           System.IO
import           System.Process
import           Network.Socket
                  (Socket, SockAddr(SockAddrUnix), socket, connect
                  , Family(AF_UNIX), SocketType(Stream), socketToHandle)
import           System.Directory
import           System.Posix.IO
import           System.Posix.Types
import           Control.Concurrent (threadDelay, forkIO)
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
  cfgPath <- readPathArg
  checkFilePath "config" cfgPath
  -- Load config.
  config <- DHL.inputFile DHL.auto cfgPath :: IO [Config]
  print config

  -- Connect to mpv socket
  putStrLn "Waiting for mpv socket..."
  waitForSocket "/tmp/mpvsocket"
  putStrLn "Connecting to mpv..."
  sockH <- connectToMPV "/tmp/mpvsocket" :: IO Handle
  putStrLn "Connected to mpv."

  -- Initialize PortMIDI
  initialize

  -- List devices
  devs <- enumerateDevices
  mapM_ (\(id, info) -> putStrLn $ show id ++ ": " ++ show (name info))
    . filter (input . snd) $ zip [0..] devs

  -- devID <- readLn
  let devID = 3

  result <- openInput devID
  case result of
    Left err -> putStrLn ("Error opening MIDI: " ++ show err)
    Right stream -> do
      putStrLn "Listening for MIDI..."

      setFilter stream filterSysex

      forever $ do
        evs <- readEvents stream
        case evs of
          Right events -> mapM_ (handleEvent sockH) events
          Left _       -> pure ()

playAt:: Handle -> String -> IO ()
playAt h timeStr = do
  sendMPV h  ("{\"command\": [\"seek\", \"" ++ timeStr ++ "\", \"absolute\"] }")
  sendMPV h  "{\"command\": [\"set_property\", \"pause\", false] }"
  return ()

handleEvent :: Handle -> PMEvent -> IO ()
handleEvent sockH (PMEvent clMsg _) =
  let 
      msg = decodeMsg clMsg
      stat = status msg
      d1 = data1 msg
      d2 = data2 msg
      chan = stat .&. 0x0F
   in case stat .&. 0xF0 of
        0x90 -> do   -- NoteOn
          if d2 == 0
            then putStrLn $ "chan:" ++ show chan
                            ++ " Note Off (via velocity 0): " ++ show d1
            else do
              putStrLn $ "chan:" ++ show chan ++ " Note On: " ++ show d1
              playAt sockH "00:00:01.5"

        0x80 -> putStrLn $ "chan:" ++ show chan ++ " Note Off: " ++ show d1
        _    -> pure ()
