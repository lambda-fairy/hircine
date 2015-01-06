-- | A bot consists of a single 'IrcModule' that listens for 'Message's
-- from the server and spits out 'Command's in response.
--
-- * Construct a new module using 'async' or 'blocking'.
--
-- * Compose existing modules together using '<>' or 'choose'.
--
-- * Run your bot using 'runIrcModule'.
--

module Hircine.Module (

    -- * The @Module@ type
    IrcModule,
    Module(),

    -- * Constructing @Module@s
    async,
    blocking,
    asyncify,

    -- * Executing @Module@s
    runIrcModule,

    -- * Utilities
    choose,
    perhaps,
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


-- | An @IrcModule@ listens for IRC 'Message's and outputs 'Command's in
-- response.
type IrcModule = Module Message [Command]


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


-- | Poll the given 'InputStream', calling the 'IrcModule' on every
-- message received.
runIrcModule :: IrcModule
    -> InputStream Message -> OutputStream Command -> IO ()
runIrcModule h is os = do
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


-- | Choose which module to run, given the input.
choose :: (Category p, Choice p)
    => (a -> Either b c) -> p b d -> p c d -> p a d
choose f p q = dimap f (either id id) $ left' p >>> right' q

-- | Transform the input, dropping it if the function returns Nothing.
perhaps :: (Category p, Choice p, Monoid (p () c))
    => (a -> Maybe b) -> p b c -> p a c
perhaps f = choose (maybeToEither . f) mempty
  where
    maybeToEither = maybe (Left ()) Right
