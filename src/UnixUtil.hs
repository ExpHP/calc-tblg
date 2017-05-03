{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module UnixUtil(
    forciblyLinkOrCopy,
    ) where

import           "base" Control.Exception
import           "extra" Control.Monad.Extra
import           "directory" System.Directory
import           "unix" System.Posix.Files

-- | Make a path readibly contain the contents of a file, in sunshine or snow.
--
-- Tries to create a hard link first, but falls back to copying if this fails
--  (for instance, if the destination is on a different filesystem)
--
-- "rm -f b && { ln -f a b || cp -f a b; }"
forciblyLinkOrCopy :: FilePath -> FilePath -> IO ()
forciblyLinkOrCopy src dest = do
    ignoreErrors (removeFile dest)
    whenM (doesPathExist dest) (fail "forciblyLinkOrCopy: Could not remove destination.")
    catch (createLink src dest)
          (\(_ :: SomeException) -> copyFile src dest)

-- | Simply horrifying.
ignoreErrors :: IO () -> IO ()
ignoreErrors = handle (\(_ :: SomeException) -> pure ())
