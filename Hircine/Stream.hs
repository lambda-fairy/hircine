{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for parsing and unparsing streams of messages.

module Hircine.Stream (
    Stream(..),
    makeStream,
    handleStream,
    ) where


import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.IO

import Hircine.Core


data Stream = Stream {
    streamReceive :: IO Message,
    streamSend :: [Command] -> IO ()
    }


-- | Convert a 'Handle' to an IRC stream.
handleStream :: Handle -> Stream
handleStream h = makeStream (B.hGetLine h) (B.hPutStr h)


-- | Build an IRC stream.
makeStream
    :: IO ByteString
        -- ^ Read a single LF-terminated line (like 'B.hGetLine')
    -> (ByteString -> IO ())
        -- ^ Write a block of data (like 'B.hPutStr')
    -> Stream
makeStream get put = Stream {
    streamReceive = parseMessages get,
    streamSend = renderCommands put
    }


parseMessages :: IO ByteString -> IO Message
parseMessages get = loop
  where
    loop = do
        s <- get
        case stripCRLF s of
            "" -> loop
            s' -> case parseMessage s' of
                Left _ -> error $ "parseMessages: invalid message: " ++ show s
                Right m -> return m

    stripCRLF = fst . B.spanEnd (\c -> c == 10 || c == 13)


renderCommands :: (ByteString -> IO ()) -> [Command] -> IO ()
renderCommands put
    = put . B.concat . concatMap (\c -> [renderCommand c, "\r\n"])
