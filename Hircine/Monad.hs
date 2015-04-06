{-# LANGUAGE RankNTypes #-}

module Hircine.Monad (

    -- * Types
    Hircine,
    HircineState(..),

    -- * Operations
    receive,
    send,
    buffer,

    -- * Running the bot
    runHircine

    ) where


import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Foldable (for_)
import Data.Function (fix)
import Data.IORef

import Hircine.Core
import Hircine.Command


type Hircine = ReaderT HircineState IO

data HircineState = HircineState {
    hsReceive :: IO Message,
    hsSend :: [Command] -> IO ()
    }


-- | Receive a single IRC message from the server.
receive :: Hircine Message
receive = ReaderT hsReceive

-- | Send an IRC command to the server.
send :: IsCommand c => c -> Hircine ()
send c = ReaderT $ \s -> hsSend s [toCommand c]

-- | Buffer up the inner 'send' calls, so that all the commands are sent
-- in one go.
buffer :: Hircine a -> Hircine a
buffer h = ReaderT $ \s -> do
    buf <- newIORef []
    r <- runReaderT h s {
        hsSend = \cs -> liftIO $
            atomicModifyIORef' buf $ \css ->
                css `seq` (cs : css, ())
        }
    -- Poison the IORef, so that no-one can touch it any more
    css <- atomicModifyIORef buf $ \css -> (error "messages already sent", css)
    let cs = concat $ reverse css
    -- Perform the reversal *before* calling hsSend
    -- This minimizes the time spent in hsSend's critical section
    cs `seq` hsSend s cs
    return r


runHircine :: Hircine () -> IO (Maybe Message) -> ([Command] -> IO ()) -> IO ()
runHircine h receive' send' = do
    sendLock <- newMVar ()
    incoming <- newEmptyMVar
    bracket
        (forkIO $ runReaderT h HircineState {
            hsReceive = takeMVar incoming,
            hsSend = \cs -> withMVar sendLock $ \_ -> send' cs
            })
        killThread
        (\_ -> fix $ \loop -> do
            m <- receive'
            for_ m $ \m' -> do
                putMVar incoming m'
                loop )
