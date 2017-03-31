{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
import           "base" Control.Arrow
-- import           "base" Control.Monad.IO.Class (liftIO)
-- import           "newtype" Control.Newtype
import qualified "transformers" Control.Monad.Trans.State.Lazy as StateT
-- import qualified "transformers" Control.Monad.Trans.State.Strict as Strict
import           "mtl" Control.Monad.State.Class(MonadState)
import qualified "mtl" Control.Monad.State.Class as MonadState
import           "transformers" Control.Monad.Trans.Class (lift)

-------------------------------------------
-- State types for various layers of the stack
newtype Top = Top Int
newtype Mid = Mid Bool
newtype Bot a = Bot a

-- (this is from 'newtype', here for the reader's convenience)
class Newtype nt t | nt -> t where
    pack :: t -> nt
    unpack :: nt -> t

op :: (Newtype nt t)=> (t -> nt) -> (nt -> t)
op = const unpack  -- 'op Constructor' gives a deconstructor

instance Newtype Top Int   where pack = Top;  unpack (Top x) = x;
instance Newtype Mid Bool  where pack = Mid;  unpack (Mid x) = x;
instance Newtype (Bot a) a where pack = Bot;  unpack (Bot x) = x;

-------------------------------------------
-- | A MonadState instance labeled by a newtype.
--
-- This lets you have multiple instances.
class (Newtype ns s, Monad m) => MonadStates ns s m | ns -> s where
    get :: (s -> ns) -> m s
    -- get c = state c (\s -> (s, s))

    put :: (s -> ns) -> s -> m ()
    -- put c s = state c (\_ -> ((), s))

    state :: (s -> ns) -> (s -> (a, s)) -> m a
    -- state c f = do
    --     s <- get c
    --     let ~(a, s') = f s
    --     put c s'
    --     return a

-- modify :: MonadStates ns s m => (s -> ns) -> (s -> s) -> m ()
-- modify c f = state c (\s -> ((), f s))
-- modify' :: MonadStates ns s m => (s -> ns) -> (s -> s) -> m ()
-- modify' c f = state c (\s -> let s' = f s in s' `seq` ((), s'))
-- gets :: MonadStates ns s m => (s -> ns) -> (s -> a) -> m a
-- gets c f = get c >>= pure . f

-------------------------------------------

-- The compiler insists on using this instance over the other
--  to satisfy MonadStates Bar
--  even with the pragmas.
--
-- Due to the additional type parameter nt, I wouldn't think this
--  is 'more specific' than the other?
instance {-# OVERLAPPABLE #-} (Newtype ns s, MonadStates ns s m) => MonadStates ns s (StateT nt m) where
    get c = lift (get c)
    put c = lift . put c
    state c = lift . state c

-- The compiler insists on not using this instance
instance {-# OVERLAPPING #-} (Newtype ns s, Monad m) => MonadStates ns s (StateT ns m) where
    get c = op c <$> StateT.get
    put c = StateT.put . c
    state c f = StateT.state (second c . f . op c)

type StateT = StateT.StateT
runStateT = flip StateT.runStateT
data Stack a = StateT Top (StateT Mid (StateT (Bot Int) IO) a) a

main = runStateT 7 $ runStateT True $ runStateT 0 $ do
    put Top 3
    put Mid False
    get Top >>= put Bot
