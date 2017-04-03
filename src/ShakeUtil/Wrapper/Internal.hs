{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
module ShakeUtil.Wrapper.Internal where

import           "base" Control.Monad
import           "shake" Development.Shake
import           ShakeUtil.Types

(.:) :: (x -> y) -> (a -> b -> x) -> (a -> b -> y)
f .: g = (f .) . g

(.::) :: (x -> y) -> (a -> b -> c -> x) -> (a -> b -> c -> y)
f .:: g = (f .:) . g

(.:::) :: (x -> y) -> (a -> b -> c -> d -> x) -> (a -> b -> c -> d -> y)
f .::: g = (f .::) . g

liftRules1 :: (MonadRules rules)=> (Rules a -> Rules b) -> rules a -> rules b
liftRules2 :: (MonadRules rules)=> (Rules a -> Rules b -> Rules c) -> rules a -> rules b -> rules c
liftAction1 :: (MonadAction action)=> (Action a -> Action b) -> action a -> action b
liftAction2 :: (MonadAction action)=> (Action a -> Action b -> Action c) -> action a -> action b -> action c

liftRules1 f a   = join $ fmap liftRules (f <$> dipIntoRules a)
liftRules2 f a b = join $ fmap liftRules (f <$> dipIntoRules a <*> dipIntoRules b)
liftAction1 f a   = join $ fmap liftAction (f <$> dipIntoAction a)
liftAction2 f a b = join $ fmap liftAction (f <$> dipIntoAction a <*> dipIntoAction b)
