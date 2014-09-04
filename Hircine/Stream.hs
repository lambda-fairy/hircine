{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for parsing and unparsing streams of messages.

module Hircine.Stream (

    -- * Important stuff
    socketToMessageStreams,

    -- * Less important stuff
    parseMessages,
    renderMessages,
    renderCommands

    ) where


import Control.Applicative
import Control.Exception (throwIO)
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Monoid
import Network (Socket)
import System.IO.Streams (InputStream, OutputStream)
import System.IO.Streams.Attoparsec (ParseException(..))
import qualified System.IO.Streams as S

import Hircine.Core


-- | Convert a socket to a pair of 'Message' streams.
socketToMessageStreams
    :: Socket -> IO (InputStream Message, OutputStream Command)
socketToMessageStreams sock = do
    (is, os) <- S.socketToStreams sock
    (,) <$> parseMessages is
        <*> renderCommands os


parseMessages :: InputStream Bytes -> IO (InputStream Message)
parseMessages = splitLines
    >=> S.mapM (either (throwIO . ParseException) return . parseMessage)


renderMessages :: OutputStream Bytes -> IO (OutputStream Message)
renderMessages = S.contramap ((<> "\r\n") . renderMessage)


renderCommands :: OutputStream Bytes -> IO (OutputStream Command)
renderCommands = renderMessages >=> S.contramap (Message Nothing)


-- | Split a byte stream into lines.
--
-- Returned lines are guaranteed to be non-empty.
--
splitLines :: InputStream Bytes -> IO (InputStream Bytes)
splitLines = S.splitOn isCRLF >=> S.filter (not . B.all isSpace)
  where
    isCRLF c = c == '\r' || c == '\n'
