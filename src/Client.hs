{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Exception (SomeException, bracket, catch)
import Control.Monad (unless)
import Data.ByteString.Char8 qualified as BS
import Data.Char (isHexDigit)
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
    fwd f from to = BS.hGetLine from >>= BS.hPutStrLn to . f >> fwd f from to

unescapeCtrl :: BS.ByteString -> BS.ByteString
unescapeCtrl = BS.pack . go . BS.unpack
  where
    go s = case span (== '\\') s of
      ([], []) -> []
      ([], c : r) -> c : go r
      (bs, 'u' : '0' : '0' : a : b : r)
        | isHexDigit a,
          isHexDigit b ->
            replicate ((length bs + 1) `div` 2) '\\' ++ ('u' : '0' : '0' : a : b : go r)
      (bs, r) -> bs ++ go r

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
