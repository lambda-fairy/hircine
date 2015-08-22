{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for parsing and unparsing streams of messages.

module Hircine.Stream (
    Stream(..),
    makeStream,
    handleStream,
    ) where


import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import System.IO
import Prelude  -- GHC 7.10

import Hircine.Core


data Stream = Stream {
    streamReceive :: IO (Maybe Message),
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


parseMessages :: IO ByteString -> IO (Maybe Message)
parseMessages get = do
    s <- fst . B.spanEnd isCRLF <$> get
    if B.null s
        then return Nothing
        else case parseMessage s of
            Left _ -> error $ "parseMessages: invalid message: " ++ show s
            Right m -> return $ Just m
  where
    isCRLF c = c == 10 || c == 13


renderCommands :: (ByteString -> IO ()) -> [Command] -> IO ()
renderCommands put
    = put . B.concat . concatMap (\c -> [renderCommand c, "\r\n"])
