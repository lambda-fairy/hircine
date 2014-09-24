-- | A framework for writing IRC bots.
--
-- A bot consists of a 'Handler' that receives IRC messages and performs
-- 'IO' in response.
--
-- * Use 'handler' to create a new event handler.
--
-- * Use '<>' or 'mconcat' to mix multiple handlers together.
--
-- * Use 'runHandler' to execute your bot.
--

module Hircine.Framework where


import Control.Applicative
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Monoid
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S

import Hircine.Core
import Hircine.Command


-- | An IRC event handler.
newtype Handler a = Handler {
    unHandler :: SendFn -> IO (Op (Action IO) a)
    }

instance Contravariant Handler where
    contramap f (Handler h) = Handler $ fmap (fmap (contramap f)) h

instance Divisible Handler where
    divide f (Handler g) (Handler h) = Handler $ liftA2 (liftA2 (divide f)) g h
    conquer = Handler $ pure $ pure $ conquer

instance Decidable Handler where
    lose f = Handler $ pure $ pure $ lose f
    choose f (Handler g) (Handler h) = Handler $ liftA2 (liftA2 (choose f)) g h


type SendFn = [Command] -> IO ()


makeHandler :: (SendFn -> IO (a -> IO ())) -> Handler a
makeHandler h = Handler $ fmap (Op . (Action .)) . h

reifyHandler :: Handler a -> SendFn -> IO (a -> IO ())
reifyHandler (Handler h) = fmap ((runAction .) . getOp) . h


-- | Lift a handler that accepts type @a@ to one that accepts @Maybe a@.
-- Any @Just@ values are handled as usual; @Nothing@ values are ignored.
perhaps :: Decidable f => f a -> f (Maybe a)
perhaps = choose (maybe (Left ()) Right) conquer


-- | Filter input.
contramapMaybe :: Decidable f => (a -> Maybe b) -> f b -> f a
contramapMaybe f = contramap f . perhaps


handler :: IsCommand a => (SendFn -> Msg a -> IO ()) -> Handler Message
handler = contramap fromMessage . perhaps . simpleHandler


simpleHandler :: (SendFn -> a -> IO ()) -> Handler a
simpleHandler h = makeHandler $ \send -> return $ \message -> h send message


asyncHandler :: Handler a -> (Handler a -> IO b) -> IO b
asyncHandler h k = bracket start end middle
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

    middle (chan, sendRef, _) = k $ simpleHandler $ \send message -> do
        putMVar sendRef send
        writeChan chan message


-- | Poll the given 'InputStream', calling the given 'Handler' on every
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


newtype Action f = Action { runAction :: f () }

instance Applicative f => Monoid (Action f) where
    mempty = Action $ pure ()
    mappend (Action a) (Action b) = Action $ a *> b
