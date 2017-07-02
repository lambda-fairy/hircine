{-# LANGUAGE RankNTypes #-}

module Hircine.Monad (

    -- * Types
    Hircine,

    -- * Operations
    receive,
    send,
    buffer,

    -- * Running the bot
    runHircine

    ) where


import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Reader
import Data.IORef

import Hircine.Core
import Hircine.Command
import Hircine.Stream


type Hircine = ReaderT Stream IO


-- | Receive a single IRC message from the server.
receive :: Hircine Message
receive = ReaderT streamReceive

-- | Send an IRC command to the server.
send :: IsCommand c => c -> Hircine ()
send c = ReaderT $ \s -> streamSend s [toCommand c]

-- | Buffer up the inner 'send' calls, so that all the commands are sent
-- in one go.
buffer :: Hircine a -> Hircine a
buffer h = ReaderT $ \s -> do
    buf <- newIORef []
    r <- runReaderT h s {
        streamSend = \cs ->
            atomicModifyIORef' buf $ \css ->
                css `seq` (cs : css, ())
        }
    -- Poison the IORef, so that no-one can touch it any more
    css <- atomicModifyIORef buf $ \css -> (error "messages already sent", css)
    let cs = concat $ reverse css
    -- Perform the reversal *before* calling hsSend
    -- This minimizes the time spent in hsSend's critical section
    cs `seq` streamSend s cs
    return r


runHircine :: Hircine a -> Stream -> IO a
runHircine h s = do
    incoming <- newEmptyMVar
    stopped <- newEmptyMVar
    bracket
        (forkFinally
            (runReaderT h Stream {
                streamReceive = takeThrowMVar incoming,
                streamSend = streamSend s
                })
            (putMVar stopped))
        (\_ -> takeThrowMVar stopped)
        (\_ -> forever $ try (streamReceive s) >>= putMVar incoming)


takeThrowMVar :: MVar (Either SomeException a) -> IO a
takeThrowMVar = takeMVar >=> either throwIO return
