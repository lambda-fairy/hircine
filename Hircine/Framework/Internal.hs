module Hircine.Framework.Internal (
    Handler(..),
    Accepts,
    makeHandler,
    makeHandler',
    liftKleisli
    ) where


import Control.Applicative
import Control.Category (Category((.), id))
import Control.Monad
import Data.Monoid
import Data.Profunctor
import Prelude hiding ((.), id)


-- | A @'Handler' a b@ accepts inputs of type @a@ and sends outputs of
-- type @b@.
newtype Handler a b = Handler {
    reifyHandler :: Accepts b -> IO (Accepts a)
    }

instance Category Handler where
    id = Handler return
    Handler f . Handler g = Handler $ f >=> g

instance Profunctor Handler where
    dimap f g (Handler h) = Handler $ fmap (. f) . h . (. g)

instance Choice Handler where
    left' (Handler h) = Handler $ \send ->
        flip either (send . Right) <$> h (send . Left)
    right' (Handler h) = Handler $ \send ->
        either (send . Left) <$> h (send . Right)

instance Functor (Handler a) where
    fmap = rmap

instance Monoid (Handler a b) where
    mempty = Handler $ const $ return $ const $ return ()
    mappend (Handler f) (Handler g) = Handler $ liftA2 (liftA2 (>>)) f g


-- | A callback that accepts values of type @a@.
type Accepts a = a -> IO ()


makeHandler :: (Accepts b -> IO (Accepts a)) -> Handler a b
makeHandler = Handler

makeHandler' :: (Accepts b -> Accepts a) -> Handler a b
makeHandler' = makeHandler . (return .)

liftKleisli :: (a -> IO b) -> Handler a b
liftKleisli = makeHandler' . (>=>)
