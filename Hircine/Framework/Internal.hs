module Hircine.Framework.Internal (
    Handler(..),
    Action(..),
    SendFn,
    makeHandler,
    makeHandler',
    reifyHandler
    ) where


import Control.Applicative
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Monoid

import Hircine.Core


-- | A 'Handler' accepts IRC 'Messages' and performs I/O operations in
-- response.
newtype Handler a = Handler {
    unHandler :: SendFn -> IO (Op (Action IO) a)
    }

-- | Use 'contramap' to transform incoming messages.
instance Contravariant Handler where
    contramap f (Handler h) = Handler $ fmap (fmap (contramap f)) h

-- | Use 'divide' to run two handlers on the same message.
instance Divisible Handler where
    divide f (Handler g) (Handler h) = Handler $ liftA2 (liftA2 (divide f)) g h
    conquer = Handler $ pure $ pure $ conquer

-- | Use 'choose' to switch dynamically between two handlers.
instance Decidable Handler where
    lose f = Handler $ pure $ pure $ lose f
    choose f (Handler g) (Handler h) = Handler $ liftA2 (liftA2 (choose f)) g h


type SendFn = [Command] -> IO ()


makeHandler :: (SendFn -> IO (a -> IO ())) -> Handler a
makeHandler h = Handler $ fmap (Op . (Action .)) . h

makeHandler' :: (SendFn -> a -> IO ()) -> Handler a
makeHandler' h = makeHandler $ \send -> return $ \message -> h send message

reifyHandler :: Handler a -> SendFn -> IO (a -> IO ())
reifyHandler (Handler h) = fmap ((runAction .) . getOp) . h


newtype Action f = Action { runAction :: f () }

instance Applicative f => Monoid (Action f) where
    mempty = Action $ pure ()
    mappend (Action a) (Action b) = Action $ a *> b
