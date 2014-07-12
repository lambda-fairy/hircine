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
import Control.Concurrent.MVar
import Data.Monoid
import Network.Simple.TCP (HostName, ServiceName, connect)
import qualified Network.Socket.ByteString as S

import Hircine.Types


-- | Run a Hircine action, connecting to the given server automatically.
hircine :: HostName -> ServiceName -> (Connection -> IO a) -> IO a
hircine host port k
    = connect host port $ \(sock, _addr) ->
        mkConnection (S.recv sock 16384) (S.sendAll sock)
            >>= k


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
--
-- Returned lines are guaranteed to be non-empty.
--
breakLines :: IO Bytes -> IO (IO Bytes)
breakLines recv = mkReader <$> newMVar B.empty
  where
    mkReader box = do
        chunk <- takeMVar box
        (line, chunk') <- loop [] chunk
        putMVar box chunk'
        return line

    -- Invariant: there are no newlines in xs
    loop buffer chunk =
        case B.findIndex isCRLF chunk of
            Nothing -> recv >>= loop (chunk : buffer)
            Just i ->
                let (front, back) = B.splitAt i chunk
                    line = B.concat . reverse $ front : buffer
                    back' = B.dropWhile isCRLF back
                in if B.null line
                    then recv >>= loop [back']  -- Discard blank lines
                    else return (line, back')

isCRLF :: Char -> Bool
isCRLF c = c == '\r' || c == '\n'
