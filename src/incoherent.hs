
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}

import           "base" Control.Monad.IO.Class (liftIO)
import           "transformers" Control.Monad.Trans.Reader(ReaderT(..))
import qualified "transformers" Control.Monad.Trans.Reader as ReaderT
import           "transformers" Control.Monad.Trans.Class (lift)
import           "mtl" Control.Monad.Reader.Class(MonadReader)

-------------------------------------------

-- | A MonadReader instance labeled by a newtype.
--
-- This lets you have multiple instances.
class (Newtype ns s, Monad m) => MonadReaders ns s m | ns -> s where
    ask :: (s -> ns) -> m s

newtype Top = Top Int
newtype Mid = Mid Bool
newtype Bot a = Bot a
data Stack a = ReaderT Top (ReaderT Mid (ReaderT (Bot Int) IO) a) a

-- errors like this:
--   "No instance for (MonadReaders Top Int IO)"
main = (>> pure ()) . flip runReaderT 7
                    . flip runReaderT True
                    . flip runReaderT 0
                    $ do
    ask Top >>= liftIO . print
    ask Mid >>= liftIO . print
    ask Bot >>= liftIO . print

-- I believe this is because the compiler is insisting
-- on using this instance over the other, even with the pragmas.
--
-- Due to the non-requirement that nt = ns, I don't think this
--  is 'more specific' than the other?
instance {-# OVERLAPPED #-} {-# INCOHERENT #-}
         (Newtype ns s, MonadReaders ns s m)
         => MonadReaders ns s (ReaderT nt m) where
    ask c = lift (ask c)

-- The compiler insists on not using this instance
instance {-# OVERLAPPING #-}
         (Newtype ns s, Monad m)
         => MonadReaders ns s (ReaderT ns m) where
    ask c = op c <$> ReaderT.ask

-------------------------------------------

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
