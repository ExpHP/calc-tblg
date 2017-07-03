{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wall #-}

module FunctorUtil(
    -- nested functor mapping
    ffmap, fffmap, ffffmap,
    ttraverse, tttraverse, ttttraverse,
    ttraverse_, tttraverse_, ttttraverse_,

    -- these horrifying names are because <**> is taken by Control.Applicative,
    --  and (<*>) has no non-operator name for us to fall back to
    --  ('ap' still has a Monad bound).
    (<<$>>), (<<<$>>>), (<<<<$>>>>),
    (<<*>>), (<<<*>>>), (<<<<*>>>>),

    -- nested functors/applicatives, in general
    CCCompose(..), mkCCCompose, getCCCompose,
    CCCCompose(..), mkCCCCompose, getCCCCompose,

    -- applicative instances that use MonadZip
    ZipWrapper(..), wrapZip, unwrapZip, unwrapZipEither,
    ZZipWrapper(..), wrapZZip, unwrapZZip,
    ZZZipWrapper(..), wrapZZZip, unwrapZZZip,
    ZZZZipWrapper(..), wrapZZZZip, unwrapZZZZip,
    ) where

import           "base" Data.Foldable
import           "base" Data.Traversable
import           "base" Control.Applicative
import           "base" Control.Monad
import           "base" Control.Monad.Zip
import           "base" Data.Functor.Compose
import           "base" Data.Functor.Classes

----------------------------------------------
-- fffffunctions for fffffunctors

ffmap :: (Functor s, Functor t) => (a -> b) -> s (t a) -> s (t b)
ffmap = fmap . fmap
fffmap :: (Functor s, Functor t, Functor u) => (a -> b) -> s (t (u a)) -> s (t (u b))
fffmap = fmap . ffmap
ffffmap :: (Functor s, Functor t, Functor u, Functor v) => (a -> b) -> s (t (u (v a))) -> s (t (u (v b)))
ffffmap = fmap . fffmap

ttraverse :: (Applicative f, Traversable s, Traversable t)=> (a -> f b) -> s (t a) -> f (s (t b))
ttraverse = traverse . traverse
tttraverse :: (Applicative f, Traversable s, Traversable t, Traversable u)=> (a -> f b) -> s (t (u a)) -> f (s (t (u b)))
tttraverse = traverse . ttraverse
ttttraverse :: (Applicative f, Traversable s, Traversable t, Traversable u, Traversable v)=> (a -> f b) -> s (t (u (v a))) -> f (s (t (u (v b))))
ttttraverse = traverse . tttraverse

ttraverse_ :: (Applicative f, Foldable s, Foldable t)=> (a -> f b) -> s (t a) -> f ()
ttraverse_ = traverse_ . traverse_
tttraverse_ :: (Applicative f, Foldable s, Foldable t, Foldable u)=> (a -> f b) -> s (t (u a)) -> f ()
tttraverse_ = traverse_ . ttraverse_
ttttraverse_ :: (Applicative f, Foldable s, Foldable t, Foldable u, Foldable v)=> (a -> f b) -> s (t (u (v a))) -> f ()
ttttraverse_ = traverse_ . tttraverse_

(<<$>>) :: (Functor s, Functor t) => (a -> b) -> s (t a) -> s (t b)
(<<$>>) = ffmap
(<<<$>>>) :: (Functor s, Functor t, Functor u) => (a -> b) -> s (t (u a)) -> s (t (u b))
(<<<$>>>) = fffmap
(<<<<$>>>>) :: (Functor s, Functor t, Functor u, Functor v) => (a -> b) -> s (t (u (v a))) -> s (t (u (v b)))
(<<<<$>>>>) = ffffmap


-- NOTE: Control.Applicative already stole the name (<**>) so we'll name these after the Monad functions
(<<*>>) :: (Applicative s, Applicative t) => s (t (a -> b)) -> s (t a) -> s (t b)
a <<*>> b = getCompose (Compose a <*> Compose b)
(<<<*>>>) :: (Applicative s, Applicative t, Applicative u) => s (t (u (a -> b))) -> s (t (u a)) -> s (t (u b))
a <<<*>>> b = getCCCompose (mkCCCompose a <*> mkCCCompose b)
(<<<<*>>>>) :: (Applicative s, Applicative t, Applicative u, Applicative v) => s (t (u (v (a -> b)))) -> s (t (u (v a))) -> s (t (u (v b)))
a <<<<*>>>> b = getCCCCompose (mkCCCCompose a <*> mkCCCCompose b)

----------------------------------------------

-- | Composition of three functors.
newtype CCCompose f g h a = CCComposeRaw
    { getCCComposeRaw :: Compose (Compose f g) h a
    } deriving (Eq, Ord, Show, Read,
                Eq1, Ord1, Show1, Read1,
                Functor, Applicative,
                Alternative,
                Foldable)

-- | Composition of four functors.
newtype CCCCompose f g h i a = CCCComposeRaw
    { getCCCComposeRaw :: CCCompose (Compose f g) h i a
    } deriving (Eq, Ord, Show, Read,
                Eq1, Ord1, Show1, Read1,
                Functor, Applicative,
                Alternative,
                Foldable)

{-
 naming is hard.  It would be nice to be consistent with Compose/getCompose,
  but we can't, because our data constructor is too awkward to use.
-}

-- unpacking function which hides ugly details about the inner representation
getCCCompose :: CCCompose f g h a -> f (g (h a))
getCCCompose = getCompose . getCompose . getCCComposeRaw

mkCCCompose :: f (g (h a)) -> CCCompose f g h a
mkCCCompose = CCComposeRaw . Compose . Compose

getCCCCompose :: CCCCompose f g h i a -> f (g (h (i a)))
getCCCCompose = getCompose . getCCCompose . getCCCComposeRaw

mkCCCCompose :: f (g (h (i a))) -> CCCCompose f g h i a
mkCCCCompose = CCCComposeRaw . mkCCCompose . Compose

-- "even cunning GeneralizedNewtypeDeriving" can't get these Traversables...
instance (Traversable f, Traversable g, Traversable h)=> Traversable (CCCompose f g h) where
    traverse f = fmap CCComposeRaw . traverse f . getCCComposeRaw
instance (Traversable f, Traversable g, Traversable h, Traversable i)=> Traversable (CCCCompose f g h i) where
    traverse f = fmap CCCComposeRaw . traverse f . getCCCComposeRaw

----------------------------------------------

-- | Create an instance of 'Applicative' that uses MonadZip.
--
-- As an unfortunate but necessary design wart, ZipWrapper provides its own
-- pointed data variant for 'pure'.  This complicates some of what would otherwise
-- be fairly straightforward tasks, such as recovering or folding the wrapped.
--
-- USER BEWARE:
--   Some instances (notably Traversable/Foldable) will produce errors on Pure.
--   The long and short of it is that I *really needed* these instances!
data ZipWrapper z a
    = Pure !a
    | Wrapped !(z a)
    deriving (Eq, Ord, Show, Read,
              Functor)

instance (Eq1 z)=> Eq1 (ZipWrapper z) where
    liftEq eq (Wrapped a) (Wrapped b) = liftEq eq a b
    liftEq eq (Pure a)    (Pure b)    = eq a b
    liftEq _ _ _ = False

-- NOTE: scary and not worth the trouble at the moment
-- instance (Ord1 z)=> Ord1 (ZipWrapper z) where
-- instance (Show1 z)=> Show1 (ZipWrapper z) where
-- instance (Read1 z)=> Read1 (ZipWrapper z) where

newtype ZZipWrapper z y a = ZZipWrapperRaw
    { unZZipWrapperRaw :: Compose (ZipWrapper z) (ZipWrapper y) a
    } deriving (Eq, -- Ord, Show, Read,
                Eq1, -- Ord1, Show1, Read1,
                Functor, Applicative,
                Foldable)

newtype ZZZipWrapper z y x a = ZZZipWrapperRaw
    { unZZZipWrapperRaw :: CCCompose (ZipWrapper z) (ZipWrapper y) (ZipWrapper x) a
    } deriving (Eq, -- Ord, Show, Read,
                Eq1, -- Ord1, Show1, Read1,
                Functor, Applicative,
                Foldable)

newtype ZZZZipWrapper z y x w a = ZZZZipWrapperRaw
    { unZZZZipWrapperRaw :: CCCCompose (ZipWrapper z) (ZipWrapper y) (ZipWrapper x) (ZipWrapper w) a
    } deriving (Eq, -- Ord, Show, Read,
                Eq1, -- Ord1, Show1, Read1,
                Functor, Applicative,
                Foldable)

wrapZip    :: z a -> ZipWrapper z a
wrapZZip   :: (Functor z)=> z (y a) -> ZZipWrapper z y a
wrapZZZip  :: (Functor z, Functor y)=> z (y (x a)) -> ZZZipWrapper z y x a
wrapZZZZip :: (Functor z, Functor y, Functor x)=> z (y (x (w a))) -> ZZZZipWrapper z y x w a
wrapZip    = Wrapped
wrapZZip   = ZZipWrapperRaw . Compose
                  . Wrapped . fmap Wrapped
wrapZZZip  = ZZZipWrapperRaw . mkCCCompose
                  . Wrapped . fmap Wrapped . ffmap Wrapped
wrapZZZZip = ZZZZipWrapperRaw . mkCCCCompose
                  . Wrapped . fmap Wrapped . ffmap Wrapped . fffmap Wrapped

-- | Unwrap a ZipWrapper, assuming it is not Pure.
unwrapZip    :: ZipWrapper z a -> Maybe (z a)
unwrapZip (Wrapped z) = Just z
unwrapZip (Pure _) = Nothing

-- | Isomorphism of ZipWrapper to Either.
unwrapZipEither    :: ZipWrapper z a -> Either a (z a)
unwrapZipEither (Wrapped z) = Right z
unwrapZipEither (Pure a) = Left a

-- | Unwrap nested ZipWrappers.  Returns Nothing if a Pure value is encountered at any depth.
unwrapZZip   :: (Traversable z)=> ZZipWrapper z y a -> Maybe (z (y a))
unwrapZZZip  :: (Traversable z, Traversable y)=> ZZZipWrapper z y x a -> Maybe (z (y (x a)))
unwrapZZZZip :: (Traversable z, Traversable y, Traversable x)=> ZZZZipWrapper z y x w a -> Maybe (z (y (x (w a))))
unwrapZZip   = (   unwrapZip
               >=> traverse unwrapZip
               ) . getCompose . unZZipWrapperRaw
unwrapZZZip  = (   unwrapZip
               >=> traverse unwrapZip
               >=> ttraverse unwrapZip
               ) . getCCCompose . unZZZipWrapperRaw
unwrapZZZZip = (   unwrapZip
               >=> traverse unwrapZip
               >=> ttraverse unwrapZip
               >=> tttraverse unwrapZip
               ) . getCCCCompose . unZZZZipWrapperRaw

instance (MonadZip z)=> Applicative (ZipWrapper z) where
    pure = Pure
    (Wrapped f) <*> (Wrapped x) = Wrapped (mzipWith ($) f x)
    (Pure f)    <*> (Wrapped x) = Wrapped (f <$> x)
    (Wrapped f) <*> (Pure x)    = Wrapped (($ x) <$> f)
    (Pure f)    <*> (Pure x)    = Pure (f x)

------------------------------------------

instance (Traversable z)=> Foldable (ZipWrapper z) where foldMap = foldMapDefault
instance (Traversable z)=> Traversable (ZipWrapper z) where
    traverse f (Wrapped z) = fmap Wrapped (traverse f z)
    traverse _ (Pure _) = error "traverse: Pure ZipWrapper: the author needs to ponder more on this."

instance (Traversable z, Traversable y)=> Traversable (ZZipWrapper z y) where
    traverse f = fmap ZZipWrapperRaw . traverse f . unZZipWrapperRaw
instance (Traversable z, Traversable y, Traversable x)=> Traversable (ZZZipWrapper z y x) where
    traverse f = fmap ZZZipWrapperRaw . traverse f . unZZZipWrapperRaw
instance (Traversable z, Traversable y, Traversable x, Traversable w)=> Traversable (ZZZZipWrapper z y x w) where
    traverse f = fmap ZZZZipWrapperRaw . traverse f . unZZZZipWrapperRaw
