{-# LANGUAGE FlexibleInstances #-}

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
import Data.Void
import System.IO.Streams (InputStream, OutputStream)
import qualified System.IO.Streams as S

import Hircine.Core
import Hircine.Command


-- | An IRC event handler.
newtype Handler a = Handler {
    reifyHandler :: ([Command] -> IO ()) -> IO (a -> IO ())
    }

instance Contravariant Handler where
    contramap f (Handler h) = Handler $ fmap (fmap (. f)) h

instance Divisible Handler where
    divide f (Handler g) (Handler h) = Handler $ \send -> do
        g' <- g send
        h' <- h send
        return $ \x -> case f x of
            (a, b) -> g' a >> h' b

    conquer = Handler $ const $ pure $ const $ pure ()

instance Decidable Handler where
    lose f = Handler $ const $ pure $ \a -> pure $ absurd (f a)

    choose f (Handler g) (Handler h) = Handler $ \send -> do
        g' <- g send
        h' <- h send
        return $ either g' h' . f

instance Monoid (Handler a) where
    mempty = conquer
    mappend = divide (\a -> (a, a))


-- | Lift a handler that accepts type @a@ to one that accepts @Maybe a@.
-- Any @Just@ values are handled as usual; @Nothing@ values are ignored.
perhaps :: Decidable f => f a -> f (Maybe a)
perhaps = choose (maybe (Left ()) Right) conquer


-- | Filter input.
contramapMaybe :: Decidable f => (a -> Maybe b) -> f b -> f a
contramapMaybe f = contramap f . perhaps


handler :: IsCommand a => (([Command] -> IO ()) -> Msg a -> IO ()) -> Handler Message
handler = contramap fromMessage . perhaps . simpleHandler


simpleHandler :: (([Command] -> IO ()) -> a -> IO ()) -> Handler a
simpleHandler h = Handler $ \send -> return $ \message -> h send message


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
