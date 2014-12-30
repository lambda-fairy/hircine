-- | A framework for writing IRC bots.
--
-- A bot consists of a 'Handler' that receives IRC messages and performs
-- 'IO' in response.
--
-- * Use 'async' or 'blocking' to create a new event handler.
--
-- * Use '<>' or 'fanin' to mix handlers together.
--
-- * Use 'runHandler' to execute your bot.
--

module Hircine.Framework (

    -- * The @Handler@ type
    Handler(),

    -- * Constructing @Handler@s
    async,
    blocking,
    asyncify,
    Accepts,

    -- * Executing @Handler@s
    runHandler,

    -- * Utilities
    lmapMaybe,
    fanin,
    module Data.Profunctor

    ) where


import Control.Category (Category(), (>>>))
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Codensity
import Data.Monoid
import Data.Profunctor
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S

import Hircine.Core
import Hircine.Framework.Internal


-- | Create a handler that runs in a separate thread.
--
-- @
-- async = 'asyncify' <=< 'blocking'
-- @
--
async :: (Accepts b -> a -> IO ()) -> Handler a b
async = asyncify . blocking


-- | Create a handler that runs in the main thread.
--
-- /Be careful with this function!/ If your handler blocks, or throws an
-- exception, then it'll bring down the whole bot with it. If in doubt,
-- use 'async' instead.
--
blocking :: (Accepts b -> a -> IO ()) -> Handler a b
blocking = makeHandler'


-- | Run an existing (blocking) 'Handler' in a separate thread.
asyncify :: Handler a b -> Handler a b
asyncify h = makeHandler $ \send -> do
    h' <- reifyHandler h send
    (chan, _) <- Codensity $ bracket (start h') end
    return $ writeChan chan
  where
    start h' = do
        -- FIXME: use a bounded channel instead
        chan <- newChan
        thread <- forkIO . forever $ readChan chan >>= h'
        return (chan, thread)
    end (_, thread) = killThread thread


-- | Poll the given 'InputStream', calling the 'Handler' on every
-- message received.
runHandler :: Handler Message [Command]
    -> InputStream Message -> OutputStream Command -> IO ()
runHandler h is os = do
    -- Since io-streams isn't thread-safe, we must guard against
    -- concurrent writes
    writeLock <- newMVar ()

    let send cs =
            -- TODO: some sort of buffering here would be nice
            withMVar writeLock $ \_ ->
                mapM_ (\c -> S.write (Just c) os) cs

    runCodensity (reifyHandler h send) $ \h' ->
        let loop = S.read is
                >>= maybe (return ()) (\message -> do
                        h' message
                        loop )
        in  loop


lmapMaybe :: (Category p, Choice p, Monoid (p () c))
    => (a -> Maybe b) -> p b c -> p a c
lmapMaybe f = lmap (maybe (Left ()) Right . f) . fanin mempty

fanin :: (Category p, Choice p) => p a c -> p b c -> p (Either a b) c
fanin p q = rmap (either id id) $ left' p >>> right' q
