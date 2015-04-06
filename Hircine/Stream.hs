{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for parsing and unparsing streams of messages.

module Hircine.Stream (
    socketToIRCStreams,
    makeIRCStreams
    ) where


import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Foldable (foldMap)
import Data.IORef
import Data.Monoid
import Data.Traversable (for)
import Network (Socket)
import qualified Network.Socket.ByteString as S

import Hircine.Core


-- | Convert a socket to a pair of IRC streams.
--
-- The returned actions are not thread-safe.
--
socketToIRCStreams :: Socket -> IO (IO (Maybe Message), [Command] -> IO ())
socketToIRCStreams sock
    = makeIRCStreams (S.recv sock 8192) (S.sendAll sock)


-- | Build IRC streams from a @recv@ and @send@ pair.
--
-- The @recv@ action should return the empty string when the stream is
-- closed.
--
-- The returned actions are not thread-safe.
--
makeIRCStreams
    :: IO ByteString  -- ^ @recv@
    -> (ByteString -> IO ())  -- ^ @send@
    -> IO (IO (Maybe Message), [Command] -> IO ())
makeIRCStreams recv send = do
    recv' <- parseMessages recv
    let send' = renderCommands send
    return (recv', send')


parseMessages :: IO ByteString -> IO (IO (Maybe Message))
parseMessages recv = do
    nextLine <- splitLines $ do
        s <- recv
        return $ if B.null s then Nothing else Just s
    return $ do
        line <- nextLine
        for line $ \s -> case parseMessage s of
            Left _ -> error $ "parseMessages: invalid message: " ++ show s
            Right m -> return m


renderCommands :: (ByteString -> IO ()) -> [Command] -> IO ()
renderCommands send = send . foldMap ((<> "\r\n") . renderCommand)


-- | Convert an action that reads blocks to an action that reads lines.
--
-- Returned lines are guaranteed to be non-empty.
--
splitLines :: IO (Maybe ByteString) -> IO (IO (Maybe ByteString))
splitLines next = process <$> newIORef (Just B.empty)
  where
    process box = do
        leftover <- readIORef box
        r <- loop [] leftover
        writeIORef box $ fmap snd r
        return $ fmap fst r

    loop :: [ByteString] -> Maybe ByteString -> IO (Maybe (ByteString, ByteString))
    loop _ Nothing = return Nothing
    loop ss (Just s) =
        case B.findIndex isCRLF s of
            Nothing -> next >>= loop (s : ss)
            Just i ->
                let (front, back) = B.splitAt i s
                    line = B.concat . reverse $ front : ss
                    back' = B.dropWhile isCRLF back
                in  if B.null line
                        then next >>= loop [back']  -- Discard empty chunks
                        else return $ Just (line, back')

isCRLF :: Char -> Bool
isCRLF c = c == '\r' || c == '\n'
