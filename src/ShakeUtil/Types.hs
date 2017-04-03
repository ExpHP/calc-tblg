{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShakeUtil.Types where

import           "base" Data.String
import           "base" Control.Monad.IO.Class(MonadIO)
import           "shake" Development.Shake(Rules,Action)
import           "transformers" Control.Monad.Trans.Reader(ReaderT)
import qualified "transformers" Control.Monad.Trans.Reader as ReaderT
import           "mtl" Control.Monad.Reader

-- Wrapper types around Shake to help implement "enter".
--
-- This is done in a couple of seemingly redundant ways:
--
-- * Functions passed into action-producing closures (via Fmts)
--   which do filename formatting.
-- * State threaded through wrappers around Rules and Action which
--   also produces such formatting functions.
--
-- The latter makes things like 'needFile' possible.
-- The former is still useful because it allows 'fmt' and 'file' to
-- exist as simple String->String functions not tied to the Monad.
-- (of course, one could easily lift these functions into context
--  in a line or two at the beginning of each recipe; but that is
--  precisely the sort of syntactic overhead that we are trying to
--  eliminate!!)
--
-- This is most certainly NOT the ideal way to implement this feature;
-- far better would be the ability to change directories in an action,
-- with the path change accounted for automatically in all dependencies
-- recorded.
--
-- Perhaps with some hacking on upstream, we can find a way to make
-- that possible.

--------------------------------------------------

-- these need to be here too to avoid circular imports >_>
type Pat = String
type FileString = String

-- NOTE: there are odd corner cases where attempting to
-- use this type somewhere may result in an "impredictive type"
-- (which GHC cannot handle)
type Fmt = forall b. (IsString b)=> Pat -> b
-- wrapper to hide Fmt's type wherever it causes issues
data Fmt_ = Fmt Fmt

-- Format functions taken by actions
data Fmts = F { fmt :: Fmt  -- simple formatter
              , file :: Fmt -- formatter that prepends the directory prefix
              }

-- The type of the RHS for most rule-generating operators.
type ActFun = FileString -> Fmts -> Act ()

-- Rules extended with an implicit directory prefix,
-- managed via 'enter'.
newtype App a = App { runApp :: ReaderT AppGlobal Rules a }
                deriving (Functor, Applicative, Monad,
                          MonadIO, MonadReader AppGlobal)

newtype Act a = Act { runAct :: ReaderT ActGlobal Action a }
                deriving (Functor, Applicative, Monad,
                          MonadIO, MonadReader ActGlobal)

data AppGlobal = AppGlobal
    -- A pattern which matches just the 'enter'ed prefix.
    -- "" for no prefix; this works nicely with (</>).
    { appPrefix :: Pat
    }

data ActGlobal = ActGlobal
    -- Format functions; indeed, likely the very same as those
    -- which were passed into the recipe that produced this Act.
    -- They are here so that shorthands like 'needFile' can exist.
    { actFmts :: Fmts
    }

actPrefix :: ActGlobal -> FileString
actPrefix ActGlobal{actFmts=F{file}} = file ""

class (Monad rules)=> MonadRules rules where
    liftRules :: Rules a -> rules a
    dipIntoRules :: rules a -> rules (Rules a)

class (Monad action)=> MonadAction action where
    liftAction :: Action a -> action a
    dipIntoAction :: action a -> action (Action a)

instance MonadRules Rules where
    liftRules = id
    dipIntoRules = pure
instance MonadAction Action where
    liftAction = id
    dipIntoAction = pure

instance MonadRules App where
    liftRules = App . ReaderT . const
    dipIntoRules (App input) = ask >>= (pure . runReaderT input)
instance MonadAction Act where
    liftAction = Act . ReaderT . const
    dipIntoAction (Act input) = ask >>= (pure . runReaderT input)
