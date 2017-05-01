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

module Band where

import           "exphp-prelude" ExpHPrelude hiding (putStr)
import           Prelude (putStr) -- *cough*
import           "base" GHC.Generics
import           "base" Control.Exception
import           "base" Data.Ratio((%))
import           "base" Data.Complex
import           "base" Data.IORef
import           "base" Data.Monoid(Sum(..))
import qualified "base" Data.List as List
import           "base" Data.List.NonEmpty(NonEmpty(..))
import qualified "base" Data.List.NonEmpty as NonEmpty
import qualified "base" System.IO as IO
import qualified "containers" Data.Set as Set
import qualified "containers" Data.Map as Map
import           "hmatrix" Numeric.LinearAlgebra(Matrix)
import qualified "hmatrix" Numeric.LinearAlgebra as Matrix
import           "filepath" System.FilePath((</>))
import           "directory" System.Directory
import qualified "vector-binary-instances" Data.Vector.Binary()
import qualified "ansi-terminal" System.Console.ANSI as Cli

-- vector-algorithms needs Unboxed, but those aren't functors, so pbbbbbt
import           "vector" Data.Vector(Vector,(!))
import qualified "vector" Data.Vector as Vector
import           "vector" Data.Vector.Unboxed(Unbox)
import qualified "vector" Data.Vector.Unboxed as UVector
import qualified "vector-algorithms" Data.Vector.Algorithms.Intro as UVector(sortBy)

import           GeneralUtil
import           JsonUtil
import           Band.Oracle.API
import           Band.NonzeroDots

-- this module abstracts fairly weakly over an 'oracle',
-- which contains all the domain-specific knowledge about how to compute the eigensystem.
--
-- This lets the code in this module work theoretically for e.g. VASP as well as phonopy.
--
-- There's no typeclass; such would be a *very premature* abstraction!
-- One must instead recompile this module, changing the following imports
--  (and possibly modifying the Strategies here to be better-suited to the new problem)
import           Band.Oracle.Phonopy(Oracle)
import qualified Band.Oracle.Phonopy as Oracle

-------------------

-- The parameters that the user must concern himself with.
data UncrossConfig = UncrossConfig
  { cfgOracleA :: Oracle
  , cfgOracleB :: Oracle
  , cfgWorkDir :: FilePath
  }

runUncross :: UncrossConfig -> IO ()
runUncross cfg@UncrossConfig{..} = do
    uncrosser <- initUncross cfg

    let vecsToPrecompute = uncrosserRequiredEigenvectors uncrosser

    -- -- HACK: compute all of them
    -- let vecsToPrecompute = [ (s,q)
    --                        | s <- uncrosserAllSystems uncrosser
    --                        , q <- uncrosserSystemKIds uncrosser s
    --                        ]

    -- Some strategies will absolutely require certain eigenvectors at some point.
    -- We can collect the eigenvectors ahead of time.
    let vecsA = vecsToPrecompute >>= \case (SystemA, q) -> [q]; _ -> []
    let vecsB = vecsToPrecompute >>= \case (SystemB, q) -> [q]; _ -> []
    _ <- precomputeOriginalVectors uncrosser SystemA vecsA
    _ <- precomputeOriginalVectors uncrosser SystemB vecsB

    permutationsA <- Vector.fromList <$> needAllPermutations uncrosser SystemA
    permutationsB <- Vector.fromList <$> needAllPermutations uncrosser SystemB

    Oracle.askToWriteCorrectedFile cfgOracleA permutationsA
    Oracle.askToWriteCorrectedFile cfgOracleB permutationsB

-- FIXME FIXME
-- HACK HACK HACK
-- Takes advantage of the existing and under-utilized infrastructure
--  for preloading eigenvectors, and under-utlizes it even more. >_>
--
-- Or equivalently: computes eigenvectors at all kpoints and yields functions to load them.
makeAnUncrosserSolelyToActAsAPrecomputedEigenvectorCache :: UncrossConfig -> IO ( [(LineId, Int)]
                                                                                , (LineId, Int) -> IO (Energies, Kets)
                                                                                , (LineId, Int) -> IO (Energies, Kets)
                                                                                )
makeAnUncrosserSolelyToActAsAPrecomputedEigenvectorCache cfg = do
    u <- initUncross cfg
    let qs = uncrosserKIds u
    _ <- precomputeOriginalVectors u SystemA qs
    _ <- precomputeOriginalVectors u SystemB qs
    let funcForSystem s q = (,) <$> (head <$> needOriginalEnergies u s [q])
                                <*> (head <$> needOriginalVectors  u s [q])

    pure $ (qs, funcForSystem SystemA, funcForSystem SystemB)


initUncross :: UncrossConfig -> IO Uncrosser
initUncross UncrossConfig{..} = do
    createDirectoryIfMissing True cfgWorkDir

    let systems = [SystemA, SystemB]

    let uncrosserWorkDir = cfgWorkDir

    let uncrosserOracle = \case SystemA -> cfgOracleA
                                SystemB -> cfgOracleB

    let !uncrosserKIds = expect "oracles have conflicting highsym line point counts"
                         $ onlyUniqueValue $ Oracle.kIds . uncrosserOracle <$> systems
    let !uncrosserNBands = expect "conflicting values for NBands"
                           $ onlyUniqueValue $ Oracle.nBands . uncrosserOracle <$> systems

    -- given that the (strict) uncrosserKIds didn't throw an exception for the above,
    -- we can safely assume both oracles will give the same answer to these:
    let uncrosserLineIds = Oracle.lineIds $ uncrosserOracle SystemA
    let uncrosserLineLength = Oracle.lineLength $ uncrosserOracle SystemA

    [refsA, refsB] <- replicateM 2 $
                        Vector.forM (Vector.fromList uncrosserLineIds) $ \h ->
                            let n = uncrosserLineLength h
                            in Vector.sequence (Vector.replicate n $ newIORef NotStarted)

    let uncrosserPermCacheRef SystemA ((LineId h), i) = refsA ! h ! i
        uncrosserPermCacheRef SystemB ((LineId h), i) = refsB ! h ! i

    let uncrosserStrategy s q@(h,_) = paranoidInterStrategy (uncrosserLineLength h) s q

    pure $ Uncrosser{..}

-------------------

data System = SystemA -- ^ The eigensystem we're generally trying to match against.
            | SystemB -- ^ The eigensystem we're generally trying to permute.
            deriving (Eq, Show, Read, Ord)

data Strategy = IdentityPerm
              | DotAgainst !System !(LineId, Int)
              | Extrapolate !Int [Int]
              deriving (Eq, Show, Ord, Read)

-- NOTE: IDEA:
--  So currently, crossings still provide trouble, even with heavy use of dot products.
--  (Need to test if they are still an issue when we use dot products 100% of the way)
--
--  If we dot against bras at a couple of q points (all nearby and on the same hsym line)
--  then for each ket we could immediately accept a match with anything that has e.g. >90% prob.

-----------------------------------------------------------------

-- Basically a set of global constants to the computation.
data Uncrosser = Uncrosser
  { uncrosserWorkDir :: FilePath
  , uncrosserStrategy :: System -> (LineId, Int) -> Strategy
  , uncrosserOracle :: System -> Oracle
  , uncrosserPermCacheRef :: System -> (LineId, Int) -> IORef (DagNode Perm)
  -- these things technically are already provided by the Oracles;
  -- they're here as well because it is expected that the oracles match!
  , uncrosserNBands :: Int
  , uncrosserKIds :: [(LineId, Int)]
  , uncrosserLineIds :: [LineId]
  , uncrosserLineLength :: LineId -> Int
  }

defaultStrategy :: Int -> System -> (LineId, Int) -> Strategy
defaultStrategy segSize = f
  where
    center = segSize `div` 2
    f SystemA (_, i) | i == center  = IdentityPerm
    f SystemB (h, i) | i == center  = DotAgainst SystemA (h, center)
    f s (h, i) | abs (center - i) == 1 = DotAgainst s (h, center)
    f _ (_, i) | abs (center - i) == 2 && center < i  = Extrapolate 2 [i-1, i-2, i-3]
    f _ (_, i) | abs (center - i) == 2 && i < center  = Extrapolate 2 [i+1, i+2, i+3]
    f _ (_, i) | abs (center - i) == 3 && center < i  = Extrapolate 3 [i-1, i-2, i-3, i-4, i-5]
    f _ (_, i) | abs (center - i) == 3 && i < center  = Extrapolate 3 [i+1, i+2, i+3, i+4, i+5]
    f _ (_, i) | center < i  = Extrapolate 3 [i-1, i-2, i-3, i-4, i-5, i-6, i-7]
    f _ (_, i) | i < center  = Extrapolate 3 [i+1, i+2, i+3, i+4, i+5, i+6, i+7]
    f _ _ = error "no ghc, I'm pretty sure this is total?"

-- eats up all your free disk space
paranoidInterStrategy :: Int -> System -> (LineId, Int) -> Strategy
paranoidInterStrategy segSize = f
  where
    center = segSize `div` 2
    f SystemB (_, i) | i == center  = IdentityPerm
    f SystemB (h, i) | center < i  = DotAgainst SystemB (h, i-1)
    f SystemB (h, i) | i < center  = DotAgainst SystemB (h, i+1)
    f SystemA q = DotAgainst SystemB q
    f _ _ = error "no ghc, I'm pretty sure this is total?"

-- eats up all your free disk space
paranoidIntraStrategy :: Int -> System -> (LineId, Int) -> Strategy
paranoidIntraStrategy segSize = f
  where
    center = segSize `div` 2
    f SystemB   (_, i) | i == center = IdentityPerm
    f SystemA q@(_, i) | i == center = DotAgainst SystemB q
    f sys (h, i) | center < i  = DotAgainst sys (h, i-1)
    f sys (h, i) | i < center  = DotAgainst sys (h, i+1)
    f _ _ = error "no ghc, I'm pretty sure this is total?"

computePermAccordingToStrategy :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> Strategy -> io Perm
computePermAccordingToStrategy u s q strat = do
    liftIO $ IO.putStrLn $ "At " ++ show (s,q) ++ ": Trying Strategy " ++ show strat
    computePermAccordingToStrategy' u s q strat

computePermAccordingToStrategy' :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> Strategy -> io Perm
computePermAccordingToStrategy' u _ _ IdentityPerm = pure $ Vector.fromList [0..uncrosserNBands u - 1]

computePermAccordingToStrategy' u ketS ketQ (DotAgainst braS braQ) = tryIt
  where
    tryIt = do
        [kets] <- needOriginalVectors u ketS [ketQ]
        [ketEs] <- needOriginalEnergies u ketS [ketQ]
        [bras] <- needUncrossedVectors u braS [braQ]
        --liftIO.traceIO $ "ready to dot: " ++ show (ketS, ketQ) ++ " :: " ++ show (braS, braQ)

        let ((degenSummary, searchSummary, assignSummary), maybeSolution) =
                dotAgainstForPerm defaultMatchTolerances bras ketEs kets
                -- dotAgainstForPerm' defaultMatchTolerances bras kets -- XXX

        showDegenSummary  degenSummary
        showSearchSummary searchSummary
        showAssignSummary assignSummary
        maybe giveUp pure maybeSolution

    giveUp = liftIO $ do
        traceIO "dotting for perm failed"
        computePermAccordingToStrategy' u ketS ketQ IdentityPerm

computePermAccordingToStrategy' u sys thisQ@(line, thisI) (Extrapolate polyOrder otherIs) = do
    otherEsByBand <- Vector.fromList . List.transpose . fmap toList
                     <$> needUncrossedEnergies u sys ((line,) <$> otherIs)
    [theseEs] <- needOriginalEnergies u sys [thisQ]
    let guessEs = fmap (\otherEs -> extrapolate polyOrder otherIs otherEs thisI) otherEsByBand

    case matchEnergiesForPerm guessEs theseEs of
        Left err -> do
            warn $ "Troublesome energies at " ++ show (sys, thisQ) ++ ": (" ++ err ++ ")"

            -- try dotting against the other system (this could create a cycle)
            let sys' = uncrosserOtherSystem u sys
            -- (fallback: try dotting against one of the points we already requested in this strategy)
            let nearQ = (line, nearest thisI otherIs)

            -- before that, let's precompute vectors around us in case there are more problem spots nearby
            _ <- needOriginalVectors u sys $ uncrosserNearbyIdsOnLine u 2 thisQ

            uncrosserWouldCreateCycle u sys' thisQ >>= \case
                False -> computePermAccordingToStrategy u sys thisQ (DotAgainst sys' thisQ)
                True  -> computePermAccordingToStrategy u sys thisQ (DotAgainst sys nearQ)

        Right x -> pure x

-- Collect points that will absolutely require eigenvectors at some point, so we can collect them in advance.
uncrosserRequiredEigenvectors :: Uncrosser -> [(System, (LineId, Int))]
uncrosserRequiredEigenvectors u = [ strategyRequiredEigenvectors s q $ uncrosserStrategy u s q
                                  | s <- uncrosserAllSystems u
                                  , q <- uncrosserKIds u
                                  ] >>= id

-- Collect points that will absolutely require eigenvectors at some point, so we can collect them in advance.
strategyRequiredEigenvectors :: System -> (LineId, Int) -> Strategy -> [(System, (LineId, Int))]
strategyRequiredEigenvectors ketS ketQ (DotAgainst braS braQ) = [(ketS,ketQ), (braS,braQ)]
strategyRequiredEigenvectors _ _ _ = []

warn :: (MonadIO io)=> String -> io ()
warn = liftIO . IO.hPutStrLn IO.stderr

nearest :: (Num a, Ord a, Foldable t)=> a -> t a -> a
nearest x = minimumBy (comparing (abs . subtract x)) . toList

uncrosserAllSystems :: Uncrosser -> [System]
uncrosserAllSystems = const [SystemA, SystemB]

uncrosserOtherSystem :: Uncrosser -> System -> System
uncrosserOtherSystem _ SystemA = SystemB
uncrosserOtherSystem _ SystemB = SystemA

-- If we did `needPermutation` or `needUncrossed{Energies,Vectors}` at this point,
--   would we create a dependency cycle?
uncrosserWouldCreateCycle  :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Bool
uncrosserWouldCreateCycle u s q =
    let ref = uncrosserPermCacheRef u s q
    in liftIO $ (BeingComputed ==) <$> readIORef ref

uncrosserNearbyIdsOnLine :: Uncrosser
                         -> Int           -- how many extra points in each direction?
                         -> (LineId, Int) -- center
                         -> [(LineId,Int)]
uncrosserNearbyIdsOnLine u n (h,i) = (h,) <$> [lo..hi-1]
  where
    lo = max (i - n) 0
    hi = min (i + n) (uncrosserLineLength u h)

-----------------------------------------------------------------
-- requesting data at kpoints

-- This is a high-level interface over the caches and the oracle, to be used by the strategies

needOriginalEnergies :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Energies]
needOriginalEnergies u s = liftIO . Oracle.askEigenvalues (uncrosserOracle u s)

-- CAUTION: Don't use with too many vectors at once!
needOriginalVectors :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Kets]
needOriginalVectors = eigenvectorCacheRequest

-- This is better to use than needOriginalVectors if you're just trying to precompute things.
precomputeOriginalVectors :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io ()
precomputeOriginalVectors = eigenvectorCacheEnsure

needAllPermutations :: (MonadIO io)=> Uncrosser -> System -> io [Perm]
needAllPermutations u s = needPermutations u s $ uncrosserKIds u

needPermutations :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Perm]
needPermutations = permutationCacheRequest

-- CAUTION: Don't use with too many vectors at once!
needUncrossedVectors :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Kets]
needUncrossedVectors u s q = zipWith Vector.backpermute
                             <$> needOriginalVectors u s q
                             <*> needPermutations u s q

needUncrossedEnergies :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Energies]
needUncrossedEnergies u s q = zipWith Vector.backpermute
                              <$> needOriginalEnergies u s q
                              <*> needPermutations u s q

-----------------------------------------------------------------
-- permutation computation, strongly tied to a cache.
--
-- The Strategies implicitly define a data dependency dag.

data DagNode a = NotStarted    -- first time seeing node (tree edge)
               | BeingComputed -- currently in the path traveled from the root (back edge!)
               | Done a        -- node already computed (forward/cross edge)
               deriving (Eq, Show, Read, Ord)

-- request a bunch of values, which may or may not already be cached
permutationCacheRequest :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Perm]
permutationCacheRequest u s q = permutationCacheEnsure u s q
                                >> mapM (permutationCacheGetSingle u s) q

-- get something that is already in the cache
permutationCacheGetSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Perm
permutationCacheGetSingle u s q =
    let ref = uncrosserPermCacheRef u s q
    in liftIO $ readIORef ref >>=
      \case
        Done x -> pure x
        _      -> error "permutationLookupSingle: buggy bug! (not computed!)"

-- make sure things are in the cache.
-- We work on them one-by-one, as some of the things may depend on each other.
permutationCacheEnsure :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io ()
permutationCacheEnsure u s = mapM_ (permutationCacheEnsureSingle u s)

permutationCacheEnsureSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io ()
permutationCacheEnsureSingle u s q =
    let ref = uncrosserPermCacheRef u s q
    in liftIO $ readIORef ref >>=
      \case
        NotStarted -> do
            writeIORef ref BeingComputed
            val <- computeSinglePermutation u s q
            -- warn $ "Finished at " ++ show (s, q)
            writeIORef ref (Done $!! val)

        BeingComputed -> error "permutationCacheEnsure: buggy bug! (dependency cycle in strategies)"
        Done _ -> pure ()

-- perform the (expensive) computation,
-- descending further down the dependency dag.
computeSinglePermutation :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Perm
computeSinglePermutation u s q = computePermAccordingToStrategy u s q
                                 $ uncrosserStrategy u s q

-----------------------------------------------------------------
-- filesystem-based cache of eigenvectors from phonopy

-- request a bunch of values, which may or may not already be cached
-- CAUTION: Asking for many kets will use a LOT of memory!
-- NOTE: Duplicates will be blindly given if requested.
eigenvectorCacheRequest :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Kets]
eigenvectorCacheRequest u s q = eigenvectorCacheEnsure u s q
                                >> mapM (eigenvectorCacheGetSingle u s) q

-- make sure things are in the cache
eigenvectorCacheEnsure :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io ()
eigenvectorCacheEnsure u s qs = do
    qs' <- List.sort . toList . Set.fromList
          <$> filterM (fmap not . eigenvectorCacheHasSingle u s) qs
    unless (null qs') $
        computeEigenvectors (eigenvectorCachePutSingle u s) u s qs' >> pure ()

eigenvectorCacheHasSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Bool
eigenvectorCacheHasSingle u s q = eigenvectorCachePath u s q >>= liftIO . doesFileExist

-- get something that is known to be in the cache
eigenvectorCacheGetSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Kets
eigenvectorCacheGetSingle u s q =
    liftIO $ eigenvectorCachePath u s q >>= fmap ketsFromCereal . readBinary

eigenvectorCachePutSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> Kets -> io ()
eigenvectorCachePutSingle u s q kets =
    liftIO $ eigenvectorCachePath u s q >>= \fp -> writeBinary fp $ ketsToCereal kets

eigenvectorCachePath :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io FilePath
eigenvectorCachePath u s (LineId h, i) = do
    let dir = uncrosserWorkDir u </> "eigenvecs"
    liftIO $ createDirectoryIfMissing True dir  -- XXX this function ought to be pure...
    pure $ dir </> concat [ show s, "-", show h, "-", show i, ".bin" ]

-- perform the (expensive) computation, streaming results into a callback.
-- We can do many points at once to alleviate against phonopy's startup time.
computeEigenvectors :: (MonadIO io)=> ((LineId, Int) -> Kets -> IO a) -> Uncrosser -> System -> [(LineId, Int)] -> io [a]
computeEigenvectors cb u s = liftIO . Oracle.askEigenvectorsVia cb (uncrosserOracle u s)

-- part of this balanced breakfast
type KetsCereal = Vector (Vector (Double, Double))

ketsFromCereal :: KetsCereal -> Kets
ketsFromCereal = fmap Vector.convert . ffmap (uncurry (:+))

ketsToCereal :: Kets -> KetsCereal
ketsToCereal = ffmap (realPart &&& imagPart) . fmap Vector.convert

ketNorm :: Ket -> Double
ketNorm x = sqrt (magnitude (x `ketDot` x))

addKet :: Ket -> Ket -> Ket
addKet = UVector.zipWith (+)

scaleKet :: Complex Double -> Ket -> Ket
scaleKet s = UVector.map (* s)

normalizeKet :: Ket -> Ket
normalizeKet x = UVector.map (/ (ketNorm x :+ 0)) x

-----------------------------------------------------------------
-- projection strategy

-- Data type for a degenerate subspace
data Subspace i = Subspace
    { subspaceBasis :: NonEmpty Ket
    , subspaceIndices :: NonEmpty i
    } deriving (Eq, Show, Read)

-- Probability of a ket belonging to a subspace.
subspaceProb :: _ => Ket -> Subspace i -> Double
subspaceProb ket = sum . fmap (`ketKetProb` ket) . subspaceBasis

subspaceRank :: (Num a) => Subspace i -> a
subspaceRank = fromIntegral . length . subspaceBasis

subspaceLinearCombo :: Subspace i -> [Complex Double] -> Ket
subspaceLinearCombo (Subspace kets _) coeffs = foldl1 addKet $ zipWith scaleKet coeffs (toList kets)

-- -- Given a normalized ket which is *almost* a member of the subspace,
-- --  construct a similar ket which IS a member, and eliminate it from the subspace
-- --  (producing a new subspace of lesser rank orthogonal to it),
-- --  relinquishing an arbitrarily chosen id.
-- subspacePopNearest :: _ => Ket -> Subspace i -> ((i, Ket), Maybe (Subspace i))
-- subspacePopNearest bra origSubspace@(Subspace kets (i:ids)) = ((i, poppedKet), Subspace otherKets ids)
--   where
--     -- FIXME gotta really iron out details to make sure conjugates are right.

--     dots = fmap (`ketDot` bra) kets

--     -- Proj[a] = sum_{i,j}  |bi> <bi|a> <a|bj> <bj|
--     projectorInKetBasis = [ [ di * conjugate dj
--                             | dj <- dots ]
--                           | di <- dots ]

--     (eigvals, eigvecs) = eigSH projectorInKetBasis

--     (poppedKet:otherKets) =
--         zip eigvals eigvecs
--         & List.sortBy (comparing $ \(x,_) -> negate x :: Double)
--         & fmap snd
--         -- FIXME justify choice of conjugate vs non-conjugate
--         & fmap (subspaceLinearCombo origSubspace)
--         & fmap normalizeKet

-- Precondition: sorted by energy
groupDegenerateSubspaces :: forall i. Double -> (Int -> i) -> Energies -> Kets -> (DegeneracyScanSummary, [Subspace i])
groupDegenerateSubspaces tol mkId es kets = result
  where
    result = foldr (<>) mempty results

    results :: [(DegeneracyScanSummary, [Subspace i])]
    results = rec $ zip3 (assertSorted $ toList es)
                         (toList kets)
                         (mkId <$> [0..])

    rec [] = []
    rec ((e,k,i):ekis) = readSubspace (e,e) (pure (k,i)) ekis

    readSubspace :: (Double, Double) -> (NonEmpty (Ket,i)) -> [(Double,Ket,i)] -> [(DegeneracyScanSummary, [Subspace i])]
    readSubspace _ g [] = [(mempty, [gToSubspace g])]
    readSubspace (eFirstAdded, eLastAdded) g ekis@((e',k',i'):ekis')
        | e' <= eFirstAdded + tol = readSubspace (eFirstAdded, e') ((k',i') NonEmpty.<| g) ekis'
        -- stop reading this subspace
        | e' <=  eLastAdded + tol = (putSummaryPair False, [gToSubspace g]) : rec ekis
        | otherwise               = (putSummaryPair  True, [gToSubspace g]) : rec ekis

    putSummaryPair isTransitive = DegeneracyScanSummary
        { degeneracyScanNontransitiveCount = if isTransitive then 0 else 1
        , degeneracyScanTotalCount         = 1
        }

    gToSubspace :: (NonEmpty (Ket,i)) -> Subspace i
    gToSubspace = uncurry Subspace . NonEmpty.unzip . NonEmpty.reverse

data DegeneracyScanSummary = DegeneracyScanSummary
    { degeneracyScanNontransitiveCount :: Sum Int -- ^ counts how many times the numerical classification of
                                                  --   degenerate bands fails to be transitive, I.E. instances of
                                                  --   energies @a,b,c@ such that @b@ is within tolerance of both
                                                  --   @a@ and @c@, but @a@ is not within tolerance of @c@.
                                                  --   (NOTE: not all instances are counted; just one per each
                                                  --     pair of subsequent subspaces)
    , degeneracyScanTotalCount :: Sum Int         -- ^ The total against which `degeneracyScanNontransitiveCount`
                                                  --   can be compared to obtain a meaningful ratio.
                                                  --   (i.e. number of pairs of subsequent subspaces; the total
                                                  --    number of subspaces identified, minus one for each separate
                                                  --    eigensystem included in the summary)
    } deriving (Eq, Show, Read)

instance Monoid DegeneracyScanSummary where
    mempty = DegeneracyScanSummary 0 0
    mappend (DegeneracyScanSummary a1 b1) (DegeneracyScanSummary a2 b2) =
        DegeneracyScanSummary (a1 <> a2) (b1 <> b2)


-- work on degenerate subspaces
dotAgainstForPerm :: MatchTolerances Double
                  -> Kets     -- bras (those we're matching against)
                  -> Energies -- ket energies  (NOTE: beware the hack in dotAgainstForPerm'; these might be fake.)
                  -> Kets     -- kets (those being permuted)
                  -> ((DegeneracyScanSummary, DotSearchSummary, DotAssignSummary), Maybe Perm) -- FIXME these summaries are getting outta hand
dotAgainstForPerm MatchTolerances{..} allBras allKetEs allKets =
    ((degenSummary, searchSummary, assignSummary), Vector.fromList <$> resultKets)
  where
    (degenSummary, allKetSpaces) = groupDegenerateSubspaces matchTolDegenerateMaxDiff KetId allKetEs allKets

    (searchSummary, allDots) =
            second (fmap (\(BraLikeId b, k, d) -> (BraId b, k, d))) -- currently, the "bra-likes" ARE the bras
            $ getNonzeroDots matchTolDotMatrixMinProb
                             subspaceProb
                             id
                             (const 1)
                             subspaceRank
                             (toList allBras)
                             allKetSpaces

    -- TODO: This can use the Hungarian algorithm, replicating degenerate subspaces for each time they appear.

    -- Greedily pair up the best dot product, then the best among what remains, etc.
    -- I don't think this is necessarily optimal, but suspect it will work fine enough.

    -- Because pretty much everything implements Ord, we explicitly annotate the
    -- types of what we're comparing, to help the type checker yell at us more.
    bestDotsFirst = List.sortBy (comparing $ \(_,_,x) -> (-x :: Double)) allDots

    initialBras   :: Set BraId
    initialSpaces :: Map KetLikeId (NonEmpty KetId)
    initialBras   = Set.fromList $ fst <$> zip (BraId <$> [0..]) (toList allBras)
    initialSpaces = Map.fromList $ zip (KetLikeId <$> [0..]) $ fmap subspaceIndices allKetSpaces

    (assignSummary, pairs) = rec mempty [] initialBras initialSpaces bestDotsFirst where

        rec summary out bras _ _ | null bras = (summary, Just out)

        -- Seemingly possible failure case where our greedy choices paint us into a corner.
        rec summary _   _    _ []            = (summary, Nothing)

        rec summary out bras spaces ((bra, subspace, w):moreDots)
            | bra      `Set.notMember` bras   = skip
            | subspace `Map.notMember` spaces = skip
            | otherwise = yield
              where
                ket = NonEmpty.head (spaces Map.! subspace)

                bras'   = Set.delete bra bras
                spaces' = Map.update (snd . NonEmpty.uncons) subspace spaces
                -- skip  = (traceShow ("skip ", bra, subspace, w) rec)            out  bras  spaces  moreDots
                -- yield = (traceShow ("yield", bra, subspace, w) rec) ((bra,ket):out) bras' spaces' moreDots
                summary' = summary `mappend` DotAssignSummary [w]
                skip  = rec summary             out  bras  spaces  moreDots
                yield = rec summary' ((bra,ket):out) bras' spaces' moreDots

    resultKets = ffmap (\(_, KetId k) -> k)
                 $ fmap (List.sortBy (comparing $ \(BraId b,_) -> b)) pairs

-- work on individual bras and kets
dotAgainstForPerm' :: MatchTolerances Double
                   -> Kets     -- bras (those we're matching against)
                   -> Kets     -- kets (those being permuted)
                  -> ((DegeneracyScanSummary, DotSearchSummary, DotAssignSummary), Maybe Perm) -- FIXME these summaries are getting outta hand
dotAgainstForPerm' tols allBras allKets =
    -- HACK: simply use the degenerate subspace code with fake, non-degenerate energies
    dotAgainstForPerm tols{matchTolDegenerateMaxDiff=0} allBras fakeEnergies allKets
  where
    fakeEnergies = fromIntegral . fst <$> Vector.indexed (Vector.convert $ Vector.head allKets)

-- newtyped indices into the eigenkets
newtype BraId = BraId Int deriving (Eq, Show, Ord, Read)
newtype KetId = KetId Int deriving (Eq, Show, Ord, Read)

sqMagnitude :: (Num a)=> Complex a -> a
sqMagnitude (a :+ b) = a*a + b*b

ketDot :: Ket -> Ket -> Complex Double
ketDot a b = UVector.sum $ UVector.zipWith (*) (UVector.map conjugate a) b
ketKetProb :: Ket -> Ket -> Double
ketKetProb a b = sqMagnitude $ ketDot a b

newtype DotAssignSummary = DotAssignSummary
    { dotAssignProbs :: [Double] -- probabilities of chosen pairs
    } deriving (Monoid, Show)

showDegenSummary :: (MonadIO io)=> DegeneracyScanSummary -> io ()
showDegenSummary DegeneracyScanSummary{..} = liftIO $ doit
  where
    doit = do
        putStr "Number of nontransitive degeneracies: "
        let color = case degeneracyScanNontransitiveCount of 0 -> Cli.Cyan
                                                             _ -> Cli.Red

        Cli.setSGR [Cli.SetConsoleIntensity Cli.BoldIntensity]
        Cli.setSGR [Cli.SetColor Cli.Foreground Cli.Vivid color]
        putStr $ show degeneracyScanNontransitiveCount
        putStr $ "/"
        putStr $ show degeneracyScanTotalCount
        Cli.setSGR [Cli.Reset]
        putStrLn ""

showSearchSummary :: (MonadIO io)=> DotSearchSummary -> io ()
showSearchSummary summary@DotSearchSummary{..} = liftIO $ doit
  where
    doit = do
        putStr "Dot Counts:"
        Cli.setSGR [Cli.SetColor Cli.Foreground Cli.Vivid Cli.Red]
        putStr "  Computed: "        >> putStr (fmtStat dotSearchDotsPerformed)
        Cli.setSGR [Cli.SetColor Cli.Foreground Cli.Vivid Cli.Cyan]
        putStr "  AlreadyFinished: " >> putStr (fmtStat dotSearchSkippedLeft)
        putStr "  ShortCircuited: "  >> putStr (fmtStat dotSearchSkippedRight)
        Cli.setSGR [Cli.Reset]
        putStrLn ""

    fmtStat :: Sum Int -> String
    fmtStat (Sum x) = show x ++ "(" ++ show (round (100 * x % dotSearchTotal summary) :: Int) ++ "%)"

showAssignSummary :: (MonadIO io)=> DotAssignSummary -> io ()
showAssignSummary (DotAssignSummary xs) = liftIO $ doit
  where
    doit = do
        (terribad,   xs1) <- pure $ partition (< 0.25) xs
        (prettyBad,  xs2) <- pure $ partition (< 0.60) xs1
        (alright,    xs3) <- pure $ partition (< 0.80) xs2
        (good,       xs4) <- pure $ partition (< 0.90) xs3
        (great,  perfect) <- pure $ partition (< 0.99) xs4

        let f = showColorIfNonzero
        putStr "Assignment Quality:"
        putStr  "  (0-25%): " >> f Cli.BoldIntensity Cli.Vivid Cli.Magenta (length terribad)
        putStr "  (25-60%): " >> f Cli.BoldIntensity Cli.Vivid Cli.Magenta (length prettyBad)
        putStr "  (60-80%): " >> f Cli.BoldIntensity Cli.Vivid Cli.Yellow  (length alright)
        putStr "  (80-90%): " >> f Cli.BoldIntensity Cli.Vivid Cli.Yellow  (length good)
        putStr "  (90-99%): " >> f Cli.BoldIntensity Cli.Vivid Cli.Cyan    (length great)
        putStr   "  (>99%): " >> f Cli.BoldIntensity Cli.Vivid Cli.Cyan    (length perfect)
        putStrLn ""

    showColorIfNonzero :: (Num a, Eq a, Show a, MonadIO io) => Cli.ConsoleIntensity -> Cli.ColorIntensity -> Cli.Color -> a -> io ()
    showColorIfNonzero _ _ _ 0 = liftIO $ putStr "0"
    showColorIfNonzero weight intensity color x = liftIO $ do
        Cli.setSGR [Cli.SetConsoleIntensity weight]
        Cli.setSGR [Cli.SetColor Cli.Foreground intensity color]
        IO.putStr (show x)
        Cli.setSGR [Cli.Reset]

-----------------------------------------------------------------
-- perturbation theory
--
-- This actually takes the eigensystem for the full hamiltonian as input,
--  so it is mostly for retrospective analysis (e.g. "is first-order perturbation theory enough?"),
--  and prove helpful as a tool in band-pairing across systems.


-- more newtypes solely because I don't trust myself
newtype Order0Id = Order0Id Int deriving (Eq, Show, Ord, Read)
newtype ExactId = ExactId Int deriving (Eq, Show, Ord, Read)

-- | Given the exact eigensystem and an unperturbed eigensystem,
--   compute the approximate solutions that would have been yielded
--   by using degenerate, time-independent perturbation theory.
--   More specifically, it yields:
--
--   * The first order approximation to the exact energies.
--   * The perturbed kets to zeroth order. (i.e. simultaneous eigenkets
--      to the unperturbed and exact hamiltonians)
--
--   The input solutions must be sorted by energy.
--
--   The output solutions are ordered primarily according to the degenerate subspaces they originated from;
--   within a given subspace, the solutions will be ordered ascendingly by energy.
firstOrderPerturb :: Double           -- ^ tolerance for degenerate eigenenergies
                  -> (Energies, Kets) -- ^ solutions to unperturbed hamiltonian
                  -> (Energies, Kets) -- ^ solutions to exact hamiltonian
                  -> (Energies, Kets)
firstOrderPerturb tol (unperturbedEs, unperturbedKets)
                      (exactEs, exactKets) = debug result
  where
    --debug = first (Vector.map (\(_,_,x) -> x) . Vector.map (ExpHPrelude.traceWith $ \(a,b,c) -> (a,b,c,b-c)) . Vector.zip3 unperturbedEs exactEs)
    debug = id

    (_degenSummary, unperturbedSubspaces) = groupDegenerateSubspaces tol Order0Id unperturbedEs unperturbedKets

    -- basically we just diagonalize the total hamiltonian in each degenerate subspace of the original

    (_searchSummary, allDots) =
            second (fmap (\  (BraLikeId k0, KetLikeId k, d :: Complex Double)
                          -> (Order0Id k0,  ExactId k,   d)))
            -- (NOTE: since KetDot takes the Bra first, allDots will contain <i0|j>, not <i|j0>)
            $ getNonzeroDots tol ketDot sqMagnitude
                             (const 1) (const 1)
                             (toList unperturbedKets)
                             (toList exactKets)

    -- collect nonzero coefficients for each unperturbed ket
    dotsMap :: Map Order0Id (Map ExactId (Complex Double))
    dotsMap = Map.fromListWith (<>)
              . fmap (\(k0,k,d) -> (k0, Map.singleton k d))
              $ allDots

    exactE :: ExactId -> Double
    exactE (ExactId i) = exactEs Vector.! i

    -- Matrix to diagonalize:
    --
    --    H_ij = <i0|H|j0> = sum_n E_n <i0|n><n|j0>
    --
    -- * |i0>, |j0> are unperturbed eigenkets
    -- * E_n, |n> describe the exact eigensystem
    -- * i,j go over degenerate subspace
    -- * n iterates over the entire basis
    matrixElement :: Order0Id -> Order0Id -> Complex Double
    matrixElement i j | i > j = conjugate $ matrixElement j i
    matrixElement i j = Map.foldl' (+) 0
                      $ Map.intersectionWithKey (\n i0Dn j0Dn -> i0Dn * conjugate j0Dn * realToFrac (exactE n))
                                                (dotsMap Map.! i) (dotsMap Map.! j)

    diagonalizeSubspace :: Subspace Order0Id -> [(Double, Ket)]
    diagonalizeSubspace subspace@Subspace{subspaceIndices} =
        second (subspaceLinearCombo subspace)
        <$> eigSH [ [ matrixElement i j
                    | j <- toList subspaceIndices ]
                  | i <- toList subspaceIndices ]

    result = (Vector.fromList *** Vector.fromList) . unzip
           $ unperturbedSubspaces >>= diagonalizeSubspace

-----------------------------------------------------------------
-- extrapolation strategy

data MatchTolerances a =
  MatchTolerances { matchTolDegenerateMaxDiff :: a    -- ^ If two bands are within (<=) this amount of energy
                                                      --   they are considered degenerate.
                  , matchTolDotMatrixMinProb :: a     -- ^ Square norm dot products smaller (<) than this are
                                                      --   dismissed from the eigenvector-dotting strategy.
                  , matchTolNonDegenerateMinDiff :: a -- ^ If two bands are within (<) this amount of energy
                                                      --   yet not within 'matchTolDegenerateMaxDiff',
                                                      --   they are deemed problematic, and eigenvectors
                                                      --   are requested.
                  , matchTolMaxGuessError :: a        -- ^ If an extrapolated value is at least this far from
                                                      --   the value it is matched against, the result is tossed
                                                      --   and eigenvectors are requested.
                  } deriving (Eq, Show, Read)

defaultMatchTolerances :: (Floating a)=> MatchTolerances a
defaultMatchTolerances =
    -- NOTE: I'm not actually sure which of the two should be smaller
    --  between matchTolNonDegenerateMinDiff and matchTolMaxGuessError.
    -- It'll come to me. (...?)
    MatchTolerances { matchTolDegenerateMaxDiff    = 0
                    , matchTolDotMatrixMinProb     = 1e-5
                    , matchTolNonDegenerateMinDiff = 1e-3
                    , matchTolMaxGuessError        = 1e-2
                    }

-- matchEnergiesForPerm guessEs trueEs  returns perm such that  trueEs `backpermute` perm
--   is ordered like guessEs.
matchEnergiesForPerm :: (Ord a, Floating a, Unbox a, Show a)
                     => Vector a -> Vector a -> Either String Perm
matchEnergiesForPerm = matchEnergiesForPermWith defaultMatchTolerances

matchEnergiesForPermWith :: (Ord a, Floating a, Unbox a, Show a)
                         => MatchTolerances a -> Vector a -> Vector a -> Either String Perm
matchEnergiesForPermWith tols guessEs' trueEs' = result
  where
    result = do
        let (guessPerm, guessEs) = sortArgSort guessEs'
        let (truePerm,  trueEs)  = sortArgSort trueEs'

        -- The only reasonable conclusion we *can* make with the limited information we have is that
        --  the sorted guesses correspond directly to the sorted energies (up to degenerate subspaces).

        -- Either this conclusion is clearly correct (and we can return it),
        -- or we are unsure (in which case we must resort to more powerful tools)
        let outPerm = truePerm `Vector.backpermute` argSort guessPerm

        () <- validateDegeneraciesAreObvious trueEs
        () <- validateGuessesAreReasonablyClose guessEs trueEs
        pure outPerm

    MatchTolerances { matchTolDegenerateMaxDiff
                    , matchTolNonDegenerateMinDiff
                    , matchTolMaxGuessError
                    } = tols

    validateDegeneraciesAreObvious xs =
        let diffs = abs <$> Vector.zipWith (-) (Vector.init xs) (Vector.tail xs)
        in case find (\x -> matchTolDegenerateMaxDiff < x && x < matchTolNonDegenerateMinDiff) diffs of
            Just x -> Left $ "Nondegenerate bands are dangerously close! (difference of " ++ show x ++ ")"
            Nothing -> Right ()

    validateGuessesAreReasonablyClose as bs =
        let diffs = abs <$> Vector.zipWith (-) as bs
        in case List.maximum diffs of
            x | matchTolMaxGuessError < x -> Left $ "Guess is too far from actual! (difference of " ++ show x ++ ")"
            _ -> Right ()

-- Get a permutation @perm@ such that @xs `backpermute` perm@ is sorted.
argSort :: (Ord a, Unbox a) => Vector a -> Perm
argSort = fst . sortArgSort

sortArgSort :: (Ord a, Unbox a) => Vector a -> (Vector Int, Vector a)
sortArgSort xs = (Vector.convert *** Vector.convert) . UVector.unzip $ UVector.create $ do
    -- Don't change the first '$' above to a '.', you'll kill type inference.
    -- the ugly monad machinery is for sortBy.
    xsi <- UVector.thaw . UVector.indexed . Vector.convert $ xs
    UVector.sortBy (comparing snd) xsi
    pure xsi

{-
Matrix equation like this:

    [ 1   (x - x0)   (x - x0)^2 ]  [ a ]     [ y0 ]
    [ 1   (x - x1)   (x - x1)^2 ]  [ b ]  =  [ y1 ]
    [ 1   (x - x2)   (x - x2)^2 ]  [ c ]     [ y2 ]

Where  a = y  is the extrapolated value at x.
-}

-- NOTE: The same matrix is solved for every band, so it is possible to adapt this to solve for
--       all bands at once. (Matrix.linearSolve takes a square matrix and a matrix of columns)
extrapolate :: (Real x, Floating y, _)=> Int -> [x] -> [y] -> x -> y
extrapolate order px py x = result
  where
    result = head -- . expect "extrapolate: degenerate matrix (duplicate x position?)"
                  $ solveLinearSystemLS mat py
    mat = [[realToFrac (p - x) ** realToFrac k | k <- [0..order]] | p <- px ]

-- least squares
solveLinearSystemLS :: (Floating a, _)=> [[a]] -> [a] -> [a]
solveLinearSystemLS a b = fmap (expect "solveLSLS: bug" . onlyValue) . Matrix.toLists
                          $ Matrix.linearSolveLS (Matrix.fromLists a)
                                                 (Matrix.asColumn . Matrix.fromList $ b)

-- From a hermitian matrix, get eigenvalues and eigenvectors in an obvious format,
--  sorted ascendingly by eigenvalue.
eigSH :: [[Complex Double]] -> [(Double, [Complex Double])]
eigSH = reverse -- Matrix.eigSH sorts descendingly by eigenvalue
      . uncurry zip
      . (Matrix.toList *** toCols) -- Matrix.eigSH gives vectors as columns
      . Matrix.eigSH . checkedSym . fromRows

-- A very paranoid constructor for Herm which would rather fail if the input is hermitian
-- than to quietly sweep it under the rug.
checkedSym :: Matrix (Complex Double) -> Matrix.Herm (Complex Double)
checkedSym m = reallyAssert (diffNorm * 1e8 <= origNorm) theSym
  where
    theSym = Matrix.sym m
    m' = Matrix.unSym theSym
    origNorm = sum . fmap sqMagnitude . concat . Matrix.toLists $ m
    diffNorm = sum . fmap sqMagnitude . concat . Matrix.toLists $ m' - m

fromRows :: (_)=> [[a]] -> Matrix a
fromRows = Matrix.fromLists
toRows :: (_)=> Matrix a -> [[a]]
toRows = Matrix.toLists

-- note tr' is the NON-conjugate transpose
fromCols :: (_)=> [[a]] -> Matrix a
fromCols = Matrix.tr' . Matrix.fromLists
toCols :: (_)=> Matrix a -> [[a]]
toCols = Matrix.toLists . Matrix.tr'

-------------------------------------------

vectorBinopImpl :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
vectorBinopImpl f a b | length a /= length b = error "vectorBinopImpl: Length mismatch"
                      | otherwise = Vector.zipWith f a b
vectorScalarImpl :: (a -> b -> c) -> Vector a -> b -> Vector c
vectorScalarImpl f = flip (scalarVectorImpl (flip f))
scalarVectorImpl :: (a -> b -> c) -> a -> Vector b -> Vector c
scalarVectorImpl f a = fmap (f a)

(>+>) :: (Num a)=> Vector a -> Vector a -> Vector a
(>+>) = vectorBinopImpl (+)
(>->) :: (Num a)=> Vector a -> Vector a -> Vector a
(>->) = vectorBinopImpl (-)
(>*>) :: (Num a)=> Vector a -> Vector a -> Vector a
(>*>) = vectorBinopImpl (*)

(@+>) :: (Num a)=> a -> Vector a -> Vector a
(@+>) = scalarVectorImpl (+)
(@->) :: (Num a)=> a -> Vector a -> Vector a
(@->) = scalarVectorImpl (-)
(@*>) :: (Num a)=> a -> Vector a -> Vector a
(@*>) = scalarVectorImpl (*)

(>+@) :: (Num a)=> Vector a -> a -> Vector a
(>+@) = vectorScalarImpl (+)
(>-@) :: (Num a)=> Vector a -> a -> Vector a
(>-@) = vectorScalarImpl (-)
(>*@) :: (Num a)=> Vector a -> a -> Vector a
(>*@) = vectorScalarImpl (*)

-------------------------------------------
