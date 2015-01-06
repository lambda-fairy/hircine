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
    pipe,

    -- * Executing @Module@s
    runIrcModule,

    -- * Utilities
    choose,
    perhaps

    ) where


import Control.Category ((>>>))
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
-- async = ('pipe' '>>>') . 'blocking'
-- @
--
async :: ((b -> IO ()) -> a -> IO ()) -> Module a b
async = (pipe >>>) . blocking


-- | Create a module that runs in the main thread.
--
-- /Be careful with this function!/ If your module blocks, or throws an
-- exception, then it'll bring down the whole bot with it. If in doubt,
-- use 'async' instead.
--
blocking :: ((b -> IO ()) -> a -> IO ()) -> Module a b
blocking = makeModule'


-- | Buffer up incoming messages, so that a long-running operation
-- downstream does not block upstream code.
pipe :: Module a a
pipe = makeModule $ \send -> do
    (chan, _) <- Codensity $ bracket (start send) end
    return $ writeChan chan
  where
    start send = do
        -- FIXME: use a bounded channel instead
        chan <- newChan
        thread <- forkIO . forever $ readChan chan >>= send
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
choose :: (a -> Either b c) -> Module b d -> Module c d -> Module a d
choose f p q = dimap f (either id id) $ left' p >>> right' q

-- | Transform the input, dropping it if the function returns Nothing.
perhaps :: (a -> Maybe b) -> Module b c -> Module a c
perhaps f = choose (maybeToEither . f) mempty
  where
    maybeToEither = maybe (Left ()) Right
