{-# LANGUAGE OverloadedStrings #-}

module Hircine.Connect
    (
      -- * Main entry point
      hircine

      -- * Utilities
    , mkConnection
    , breakLines

    ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.Monoid
import Network.Simple.TCP (HostName, ServiceName, connect)
import qualified Network.Socket.ByteString as S

import Hircine.Types


-- | Run a Hircine action, connecting to the given server automatically.
hircine :: HostName -> ServiceName -> Hircine a -> IO a
hircine host port m
    = connect host port $ \(sock, _addr) ->
        mkConnection (S.recv sock 16384) (S.sendAll sock)
            >>= runHircine m


-- | Build a connection from a @recv@ and @send@ pair.
mkConnection
    :: IO Bytes  -- ^ @recv@
    -> (Bytes -> IO ())  -- ^ @send@
    -> IO Connection
mkConnection recv send = do
    recv' <- breakLines recv
    let send' = send . (<> "\r\n")
    return $ Connection recv' send'

-- | Convert an action that reads blocks to an action that reads lines.
breakLines :: IO Bytes -> IO (IO Bytes)
breakLines recv = loop <$> newIORef B.empty
  where
    loop r = do
        buffer <- readIORef r
        case B.findIndex isCRLF buffer of
            Nothing -> do
                append <- recv
                writeIORef r $! buffer <> append
                loop r
            Just i -> do
                let (line, buffer') = B.splitAt i buffer
                writeIORef r $! B.dropWhile isCRLF buffer'
                return line

isCRLF :: Char -> Bool
isCRLF c = c == '\r' || c == '\n'
