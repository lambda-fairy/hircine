module Hircine.Module.Internal (
    Module(),
    Accepts,
    makeModule,
    makeModule',
    reifyModule,
    liftKleisli
    ) where


import Control.Applicative
import Control.Category (Category((.), id))
import Control.Monad
import Control.Monad.Codensity
import Data.Monoid
import Data.Profunctor
import Prelude hiding ((.), id)


-- | A @'Module' a b@ accepts inputs of type @a@ and sends outputs of
-- type @b@.
newtype Module a b = Module {
    reifyModule :: Accepts b -> Codensity IO (Accepts a)
    }

instance Category Module where
    id = Module return
    Module f . Module g = Module $ f >=> g

instance Profunctor Module where
    dimap f g (Module h) = Module $ fmap (. f) . h . (. g)

instance Choice Module where
    left' (Module h) = Module $ \send ->
        flip either (send . Right) <$> h (send . Left)
    right' (Module h) = Module $ \send ->
        either (send . Left) <$> h (send . Right)

instance Functor (Module a) where
    fmap = rmap

instance Monoid (Module a b) where
    mempty = Module $ const $ return $ const $ return ()
    mappend (Module f) (Module g) = Module $ liftA2 (liftA2 (>>)) f g


-- | A callback that accepts values of type @a@.
type Accepts a = a -> IO ()


makeModule :: (Accepts b -> Codensity IO (Accepts a)) -> Module a b
makeModule = Module

makeModule' :: (Accepts b -> Accepts a) -> Module a b
makeModule' = makeModule . (return .)

liftKleisli :: (a -> IO b) -> Module a b
liftKleisli = makeModule' . (>=>)
