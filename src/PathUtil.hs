{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}

module PathUtil(
    makeRelativeEx,
    testSuite_0d4f6c7d_e418_4f21_be78_22d0210becbc,
    ) where

import           "base" Data.Functor.Identity
import           "base" Debug.Trace
import qualified "directory" System.Directory as Directory
import           "shake" Development.Shake.FilePath(normaliseEx, (</>), splitDirectories, takeDirectory, takeFileName, joinPath)
import           TestUtil

-- Like makeRelative, except it isn't afraid to add "..".
makeRelativeEx :: FilePath -> FilePath -> IO FilePath
makeRelativeEx = makeRelativeExImpl

-- For mocking out the filesystem
class CanonicalizePath m where
    canonicalize :: FilePath -> m FilePath

instance CanonicalizePath IO       where
    -- should handle things like "." and ".." components, repeated slashes, and should follow symlinks.
    -- should produce absolute paths
    canonicalize = Directory.canonicalizePath

-- a mock instance so we can test without IO
instance CanonicalizePath Identity where
    canonicalize ('.':'.':'/':s) = pure $ "/some/test/" ++ s
    canonicalize ('.':'/':s)     = pure $ "/some/test/dir/" ++ s
    canonicalize s               = pure $ "/some/test/dir/" ++ s

makeRelativeExImpl :: (Monad m, CanonicalizePath m)=> FilePath -> FilePath -> m FilePath
makeRelativeExImpl x' y' = do
    x <- splitDirectories <$> canonicalize x'
    y <- splitDirectories <$> canonicalize y'
    return $ joinPath $ f x y
    where
        f [_] ys = ys
        f (x:xs) (y:ys)
            | x == y = f xs ys
            | otherwise = (".." <$ xs) ++ (y:ys)
        f _ ys = ys

testSuite_0d4f6c7d_e418_4f21_be78_22d0210becbc :: TestTree
testSuite_0d4f6c7d_e418_4f21_be78_22d0210becbc =

    -- FIXME these tests don't test canonicalization yet,
    --       because it would be annoying to incorporate everything
    --          (including fake symlinks) into the Identity mock
    "makeRelativeEx" ~:

    [ "self" ~:                   makeRelativeExImpl "foo"       "foo"   @?= Identity "foo"
    , "two basenames" ~:          makeRelativeExImpl "foo"       "bar"   @?= Identity "bar"
    , "self in subdir" ~:         makeRelativeExImpl "a/foo"     "a/foo" @?= Identity "foo"
    , "basenames in subdir" ~:    makeRelativeExImpl "a/foo"     "a/bar" @?= Identity "bar"
    , "no transform" ~:           makeRelativeExImpl "foo"       "bar/a" @?= Identity "bar/a"
    , "regression test 1" ~:      makeRelativeExImpl "foo/bar/a" "bar/a" @?= Identity "../../bar/a"
    ]
