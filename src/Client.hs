{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (forever, unless)
import Data.ByteString.Char8 qualified as BS
import Network.Socket
import System.Environment (getExecutablePath)
import System.FilePath (replaceFileName)
import System.IO
import System.Process (spawnProcess)

sockPath :: FilePath
sockPath = "/tmp/specter.sock"

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering >> hSetBuffering stdout LineBuffering >> ensureDaemon
  bracket (socket AF_UNIX Stream 0) close $ \sock -> do
    connect sock (SockAddrUnix sockPath)
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    race_ (fwd unescapeCtrl stdin h) (fwd id h stdout)
  where
    fwd f from to = forever $ BS.hGetLine from >>= BS.hPutStrLn to . f

-- MCP frameworks double-escape: \r becomes \\r in JSON stream
-- Convert known escapes to unicode so daemon's JSON parser sees control chars
-- Unknown escapes pass through unchanged (safe for JSON)
unescapeCtrl :: BS.ByteString -> BS.ByteString
unescapeCtrl = BS.pack . go . BS.unpack
  where
    go [] = []
    go ('\\' : '\\' : '\\' : '\\' : r) = '\\' : '\\' : go r
    go ('\\' : '\\' : 'r' : r) = '\\' : 'u' : '0' : '0' : '0' : 'd' : go r
    go ('\\' : '\\' : 'n' : r) = '\\' : 'u' : '0' : '0' : '0' : 'a' : go r
    go ('\\' : '\\' : 't' : r) = '\\' : 'u' : '0' : '0' : '0' : '9' : go r
    go (c : r) = c : go r

ensureDaemon :: IO ()
ensureDaemon = tryConnect >>= (`unless` startDaemon)

tryConnect :: IO Bool
tryConnect = go `catch` \(_ :: SomeException) -> pure False
  where
    go = bracket (socket AF_UNIX Stream 0) close $ \s -> connect s (SockAddrUnix sockPath) >> pure True

startDaemon :: IO ()
startDaemon = do
  daemon <- (`replaceFileName` "specterd") <$> getExecutablePath
  _ <- spawnProcess daemon []
  wait (50 :: Int)
  where
    wait 0 = pure ()
    wait n = threadDelay 100000 >> tryConnect >>= (`unless` wait (n - 1))
