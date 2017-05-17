{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Phonopy.EigenvectorCache(
    initCache,
    withCache,
    ) where

import           "exphp-prelude" ExpHPrelude hiding (putStr)
import           "base" Data.Complex
import qualified "base" System.IO as String
import           "extra" System.IO.Extra(fileEq,withTempDir)
import           "extra" Control.Monad.Extra
import qualified "containers" Data.Map as Map
import           "filepath" System.FilePath((</>))
import           "directory" System.Directory
import qualified "vector-binary-instances" Data.Vector.Binary()
import qualified "vector" Data.Vector as Vector

import           JsonUtil(readBinary, writeBinary)
import           Band.Aliases(Kets)

import           Phonopy.Types
import qualified Phonopy.IO as Phonopy(unsafeComputeEigenvectors)

-----------------------------------------------------------------
-- filesystem-based cache of eigenvectors from phonopy

type Cache = QPathData (IO Kets)

initCache :: [(FilePath, FilePath)] -- ^ [(targetBasename, sourcePath)]
          -> [String]               -- ^ additional args to phonopy (beyond "--eigenvectors")
          -> QPath                  -- ^ qpoints
          -> FilePath               -- ^ cache root (an empty or non-existing dir, or an extant cache)
          -> IO ()
initCache = initPotentiallyPreExistingCache

initPotentiallyPreExistingCache :: [(FilePath,FilePath)] -> [String] -> QPath -> FilePath -> IO ()
initPotentiallyPreExistingCache phonopyInputFiles additionalArgs qPath root = do
    createDirectoryIfMissing True root
    createDirectoryIfMissing True (pathInputDir root)
    createDirectoryIfMissing True (pathDataDir root)
    withCache root $
        \case Nothing -> error "initCache: Cache already exists and is locked!"
              (Just _) -> unlessM (andM [argsMatch, filesMatch, qPointsMatch])
                          $ initNewCache phonopyInputFiles additionalArgs qPath root

  where
    argsMatch = (additionalArgs ==) . read <$> readFile (pathArgs root)
    qPointsMatch = (qPath ==) . read <$> readFile (pathQPath root)
    filesMatch =
        let newSources = Map.fromListWithKey (\k _ _ -> error $ "initCache: Duplicate inputs for file: " <> k)
                       $ phonopyInputFiles
        in listDirectory (pathInputDir root) >>=
              allM (\name -> fileEq (pathInputDir root </> name)
                                    (newSources Map.! name))

initNewCache :: [(FilePath,FilePath)] -> [String] -> QPath -> FilePath -> IO ()
initNewCache phonopyInputFiles additionalArgs qPath root = do
    String.writeFile (pathArgs  root) (show additionalArgs)
    String.writeFile (pathQPath root) (show qPath)
    renewPath (pathInputDir root)
    forM_ phonopyInputFiles $
        \(destname, src) -> copyFile src (pathInputDir root </> destname)

-- | Assume control of the cache, locking it.
--
--   It is not safe for multiple IO actions from the Cache to be run in parallel.
withCache :: FilePath -> (Maybe Cache -> IO a) -> IO a
withCache root f =
    withLockFile (pathLockFile root) $
        \case True  -> loadCache root >>= f . Just
              False -> f Nothing

loadCache :: FilePath -> IO Cache
loadCache root = do
    args <- read <$> readFile (pathArgs root)
    qPath <- read <$> readFile (pathQPath root)
    let cache = CacheInfo root qPath args
    pure $ flip fmap (qPathIndexed qPath) $ \(key,_) ->
        cacheSelectRegionWithKey cache key >> readKey root key

pathArgs, pathQPath, pathInputDir, pathDataDir, pathLockFile :: FilePath -> FilePath
pathArgs = (</> "args")
pathQPath = (</> "qpath")
pathInputDir = (</> "input")
pathDataDir = (</> "data")
pathLockFile = (</> "lock")

withLockFile :: FilePath -> (Bool -> IO a) -> IO a
withLockFile fp act = do
    doesPathExist fp >>=
        \case True  -> act False
              False -> do
                  writeFile fp ""
                  act True `finally` removeFile fp

--------------------------------------------------------------------------

-- part of this balanced breakfast
type KetsCereal = Vector (Vector (Double, Double))

ketsFromCereal :: KetsCereal -> Kets
ketsFromCereal = fmap Vector.convert . fmap (fmap (uncurry (:+)))

ketsToCereal :: Kets -> KetsCereal
ketsToCereal = fmap (fmap (realPart &&& imagPart)) . fmap Vector.convert

keyPath :: FilePath -> (LineId, Int) -> FilePath
keyPath root (LineId h, i) = pathDataDir root </> concat [ show h, "-", show i, ".bin" ]
writeKey :: FilePath -> (LineId, Int) -> Kets -> IO ()
writeKey root key kets = writeBinary (keyPath root key) $ ketsToCereal kets
readKey :: FilePath -> (LineId, Int) -> IO Kets
readKey root key = ketsFromCereal <$> readBinary (keyPath root key)

--------------------------------------------------------------------------

data CacheInfo = CacheInfo
    { cacheRoot  :: FilePath
    , cacheQPath :: QPath
    , cacheArgs  :: [String]
    }

cacheSelectRegionWithKey :: CacheInfo -> (LineId, Int) -> IO ()
cacheSelectRegionWithKey cache@CacheInfo{..} key = do
    unlessM (doesPathExist (keyPath cacheRoot key)) $ do
        -- for now let's just evict everything and start anew
        cacheClear cache
        cacheComputeTheseKeys cache $
            filter ((fst key ==) . fst) (qPathAllIds cacheQPath)

    unlessM (doesPathExist (keyPath cacheRoot key)) $ do
        error "cacheSelectRegionWithKey: buggy bug: failed to create file for key!"

cacheClear :: CacheInfo -> IO ()
cacheClear CacheInfo{..} = let d = pathDataDir cacheRoot
                           in removePathForcibly d >> createDirectory d

cacheComputeTheseKeys :: CacheInfo -> [(LineId, Int)] -> IO ()
cacheComputeTheseKeys CacheInfo{..} qs = do
  withTempDirFromTemplate (pathInputDir cacheRoot) $ \tmp -> do
    -- compute in one step
    vecs <- Phonopy.unsafeComputeEigenvectors cacheArgs tmp
                                              (fmap (cacheQPath `qPathAt`) qs)
    -- write the cache files
    zipWithM_ (writeKey cacheRoot) qs vecs
  where
    -- NOTE: flat templates only (not a recursive copy)
    withTempDirFromTemplate template act = withTempDir $ \tmp -> do
        listDirectory template >>= \names -> do
            forM_ names $ \name -> copyFile (template </> name) (tmp </> name)
            act tmp


--------------------------------------------------------------------------

renewPath :: FilePath -> IO ()
renewPath d = removePathForcibly d >> createDirectory d
