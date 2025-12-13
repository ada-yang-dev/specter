{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException)
import Control.Exception qualified as E
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.ByteString.Char8 qualified as BC8
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Network.Socket
import System.Environment (getArgs)
import System.IO

sockPath = "/tmp/specter.sock"

main =
  getArgs >>= \case
    ["list"] -> callDaemon "_list"
    ["purge"] -> callDaemon "_purge"
    _ -> putStrLn "usage: specterctl list | specterctl purge"

callDaemon :: Text -> IO ()
callDaemon method = do
  sock <- socket AF_UNIX Stream 0
  E.try @SomeException (connect sock (SockAddrUnix sockPath)) >>= \case
    Left _ -> putStrLn "daemon not running"
    Right () -> do
      h <- socketToHandle sock ReadWriteMode
      hSetBuffering h LineBuffering
      BL.hPutStrLn h $ encode $ object ["jsonrpc" .= ("2.0" :: Text), "id" .= (1 :: Int), "method" .= method]
      line <- BL.fromStrict <$> BC8.hGetLine h
      TIO.putStrLn $ extractText line
      hClose h
  where
    extractText line = fromMaybe "error" $ do
      Object o <- decode line
      Object r <- parseMaybe (.: "result") o
      Array arr <- parseMaybe (.: "content") r
      [Object c] <- pure $ toList arr
      String s <- parseMaybe (.: "text") c
      pure s
