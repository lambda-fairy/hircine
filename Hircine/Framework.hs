-- | A framework for writing IRC bots.
--
-- A bot consists of a 'Handler' that receives IRC messages and performs
-- 'IO' in response.
--
-- * Use 'asyncHandler' or 'blockingHandler' to create a new event handler.
--
-- * Use 'Data.Monoid.<>' or 'divide' or 'choose' to mix multiple
--   handlers together.
--
-- * Use 'runHandler' to execute your bot.
--

module Hircine.Framework (

    -- * The @Handler@ type
    Handler(),

    -- * Constructing @Handler@s
    asyncHandler,
    blockingHandler,
    async,
    SendFn,

    -- * Executing @Handler@s
    runHandler,

    -- * Filtering messages
    contramapMaybe,
    perhaps,
    listenOutput

    ) where


import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Data.Functor.Contravariant.Divisible
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S

import Hircine.Core
import Hircine.Command
import Hircine.Framework.Internal


-- | Create a handler that runs in a separate thread.
--
-- @
-- asyncHandler = 'async' . 'blockingHandler'
-- @
--
asyncHandler :: IsCommand a => (SendFn -> Msg a -> IO ()) -> (Handler Message -> IO b) -> IO b
asyncHandler = async . blockingHandler


-- | Create a handler that runs in the main thread.
--
-- /Be careful with this function!/ If your handler blocks, or throws an
-- exception, then it'll bring down the whole bot with it. If in doubt,
-- use 'asyncHandler' instead.
--
blockingHandler :: IsCommand a => (SendFn -> Msg a -> IO ()) -> Handler Message
blockingHandler = contramapMaybe fromMessage . makeHandler'


-- | Run an existing 'Handler' in a separate thread.
async :: Handler a -> (Handler a -> IO b) -> IO b
async h k = bracket start end middle
  where

    start = do
        -- FIXME: use a bounded channel instead
        chan <- newChan
        sendRef <- newEmptyMVar
        thread <- forkIO $ do
            send <- takeMVar sendRef
            h' <- reifyHandler h send
            forever $ readChan chan >>= h'
        return (chan, sendRef, thread)

    end (_, _, thread) = killThread thread

    middle (chan, sendRef, _) = k $ makeHandler $ \send -> do
        putMVar sendRef send
        return $ writeChan chan


-- | Poll the given 'InputStream', calling the 'Handler' on every
-- message received.
runHandler :: Handler Message -> InputStream Message -> OutputStream Command -> IO ()
runHandler h is os = do
    -- Since io-streams isn't thread-safe, we must guard against
    -- concurrent writes
    writeLock <- newMVar ()

    let send cs =
            withMVar writeLock $ \_ ->
                mapM_ (\c -> S.write (Just c) os) cs

    h' <- reifyHandler h send

    let loop = S.read is
            >>= maybe (return ()) (\message -> do
                    h' message
                    loop )

    loop


-- | @contramapMaybe@ is a version of
-- 'Data.Functor.Contravariant.contramap' which can throw out elements.
-- If the given function returns @Nothing@, the input is dropped.
contramapMaybe :: Decidable f => (a -> Maybe b) -> f b -> f a
contramapMaybe f = choose (maybe (Left ()) Right . f) conquer


-- | @perhaps@ converts a handler of type @a@ to a handler of type
-- @Maybe a@. Any @Just@ values are passed to the underlying handler;
-- @Nothing@ values are ignored.
--
-- @
-- perhaps = 'contramapMaybe' id
-- @
--
perhaps :: Decidable f => f a -> f (Maybe a)
perhaps = contramapMaybe id
