-- | A bot consists of a single 'Module' that listens for 'Message's
-- from the server and spits out 'Command's in response.
--
-- * Construct a new module using 'async' or 'blocking'.
--
-- * Compose existing modules together using '<>' or 'fanin'.
--
-- * Run your bot using 'runModule'.
--

module Hircine.Module (

    -- * The @Module@ type
    Module(),

    -- * Constructing @Module@s
    async,
    blocking,
    asyncify,

    -- * Executing @Module@s
    runModule,

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
import Hircine.Module.Internal


-- | Create a module that runs in a separate thread.
--
-- @
-- async = 'asyncify' . 'blocking'
-- @
--
async :: ((b -> IO ()) -> a -> IO ()) -> Module a b
async = asyncify . blocking


-- | Create a module that runs in the main thread.
--
-- /Be careful with this function!/ If your module blocks, or throws an
-- exception, then it'll bring down the whole bot with it. If in doubt,
-- use 'async' instead.
--
blocking :: ((b -> IO ()) -> a -> IO ()) -> Module a b
blocking = makeModule'


-- | Run an existing (blocking) 'Module' in a separate thread.
asyncify :: Module a b -> Module a b
asyncify h = makeModule $ \send -> do
    h' <- reifyModule h send
    (chan, _) <- Codensity $ bracket (start h') end
    return $ writeChan chan
  where
    start h' = do
        -- FIXME: use a bounded channel instead
        chan <- newChan
        thread <- forkIO . forever $ readChan chan >>= h'
        return (chan, thread)
    end (_, thread) = killThread thread


-- | Poll the given 'InputStream', calling the 'Module' on every
-- message received.
runModule :: Module Message [Command]
    -> InputStream Message -> OutputStream Command -> IO ()
runModule h is os = do
    -- Since io-streams isn't thread-safe, we must guard against
    -- concurrent writes
    writeLock <- newMVar ()

    let send cs =
            -- TODO: some sort of buffering here would be nice
            withMVar writeLock $ \_ ->
                mapM_ (\c -> S.write (Just c) os) cs

    runCodensity (reifyModule h send) $ \h' ->
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
