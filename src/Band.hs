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

module Band(
    runUncross,
    firstOrderPerturb,
    UncrossConfig(..),
    ) where

import           "exphp-prelude" ExpHPrelude hiding (putStr)
import           Prelude (putStr) -- *cough*
import           "base" Data.Ratio((%))
import           "base" Data.Complex
import           "base" Data.IORef
import           "base" Control.Exception
import           "base" Data.Monoid(Sum(..))
import qualified "base" Data.List as List
import           "base" Data.List.NonEmpty(NonEmpty(..))
import qualified "base" Data.List.NonEmpty as NonEmpty
import qualified "base" System.IO as IO
import qualified "containers" Data.Set as Set
import qualified "containers" Data.Map as Map
import           "hmatrix" Numeric.LinearAlgebra(Matrix)
import qualified "hmatrix" Numeric.LinearAlgebra as Matrix
import           "directory" System.Directory
import qualified "vector-binary-instances" Data.Vector.Binary()
import qualified "ansi-terminal" System.Console.ANSI as Cli
import           "transformers" Control.Monad.Trans.Writer.Strict(WriterT,runWriterT,tell)
import           "Hungarian-Munkres" Algorithms.Hungarian(hungarian)

-- vector-algorithms needs Unboxed, but those aren't functors, so pbbbbbt
import           "vector" Data.Vector(Vector,(!))
import qualified "vector" Data.Vector as Vector
import qualified "vector" Data.Vector.Unboxed as UVector

import           GeneralUtil(expect, onlyUniqueValue, assertSorted, reallyAssert)
import           Band.NonzeroDots
import           Band.Aliases(Energies, Kets, Ket, Perm)

import           Phonopy.Types

-------------------

-- The parameters that the user must concern himself with.
data UncrossConfig = UncrossConfig
  { cfgQPath             :: QPath
  , cfgOriginalEnergiesA :: QPathData Energies -- FIXME not so much "config" as "input"...
  , cfgOriginalEnergiesB :: QPathData Energies
  , cfgOriginalVectorsA  :: QPathData (IO Kets) -- FIXME not so much "config" as "input"...
  , cfgOriginalVectorsB  :: QPathData (IO Kets)
  , cfgWorkDir :: FilePath
  }

runUncross :: UncrossConfig -> IO (Vector Perm, Vector Perm)
runUncross cfg = do
    (result, (degenSummary,searchSummary,assignSummary)) <- runWriterT $ runUncross_ cfg

    putStrLn "====TOTAL SUMMARY===="
    showDegenSummary  degenSummary
    showSearchSummary searchSummary
    showAssignSummary assignSummary
    pure result


runUncross_ :: UncrossConfig -> Compute (Vector Perm, Vector Perm)
runUncross_ cfg@UncrossConfig{..} = do
    uncrosser <- initUncross cfg

    -- -- HACK: compute all of them
    -- let vecsToPrecompute = [ (s,q)
    --                        | s <- uncrosserAllSystems uncrosser
    --                        , q <- uncrosserSystemKIds uncrosser s
    --                        ]

    permutationsA <- Vector.fromList <$> needAllPermutations uncrosser SystemA
    permutationsB <- Vector.fromList <$> needAllPermutations uncrosser SystemB

    pure (permutationsA, permutationsB)

initUncross :: UncrossConfig -> Compute Uncrosser
initUncross UncrossConfig{..} = do
    liftIO $ createDirectoryIfMissing True cfgWorkDir

    let systems = [SystemA, SystemB]

    let uncrosserWorkDir = cfgWorkDir

    let uncrosserQPath = cfgQPath
    let !_ = assert (qPathsCompatible uncrosserQPath cfgOriginalEnergiesA) ()
    let !_ = assert (qPathsCompatible uncrosserQPath cfgOriginalEnergiesB) ()

    let uncrosserOriginalEnergies = system cfgOriginalEnergiesA cfgOriginalEnergiesB
    let uncrosserOriginalVectors  = system cfgOriginalVectorsA  cfgOriginalVectorsB
    let uncrosserKIds = qPathAllIds uncrosserQPath
    let !uncrosserNBands = expect "conflicting values for NBands"
                           . onlyUniqueValue
                           . fmap (length . Vector.head . Vector.head . qPathDataByLine)
                           . fmap uncrosserOriginalEnergies $ systems

    -- given that the (strict) uncrosserKIds didn't throw an exception for the above,
    -- we can safely assume both oracles will give the same answer to these:
    let uncrosserLineIds = qPathLineIds $ uncrosserQPath
    let uncrosserLineLength = qPathLineLength $ uncrosserQPath

    [refsA, refsB] <- replicateM 2 $
                        Vector.forM (Vector.fromList uncrosserLineIds) $ \h ->
                            let len = uncrosserLineLength h
                            in Vector.sequence (Vector.replicate len . liftIO . newIORef $ NotStarted)

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
              deriving (Eq, Show, Ord, Read)

system :: a -> a -> System -> a
system a _ SystemA = a
system _ b SystemB = b

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
  , uncrosserPermCacheRef :: System -> (LineId, Int) -> IORef (DagNode Perm)
  -- these things technically are already provided by the Oracles;
  -- they're here as well because it is expected that the oracles match!
  , uncrosserNBands :: Int
  , uncrosserOriginalEnergies :: System -> QPathData Energies
  , uncrosserOriginalVectors  :: System -> QPathData (IO Kets)
  -- NOTE this is unusual; now that we are using the eigenvector cache,
  --      the uncrosser never actually needs the qpoints!
  , uncrosserQPath :: QPath
  , uncrosserKIds :: [(LineId, Int)]
  , uncrosserLineIds :: [LineId]
  , uncrosserLineLength :: LineId -> Int
  }

paranoidInterStrategy :: Int -> System -> (LineId, Int) -> Strategy
paranoidInterStrategy segSize = f
  where
    center = segSize `div` 2
    f SystemB (_, i) | i == center  = IdentityPerm
    f SystemB (h, i) | center < i  = DotAgainst SystemB (h, i-1)
    f SystemB (h, i) | i < center  = DotAgainst SystemB (h, i+1)
    f SystemA q = DotAgainst SystemB q
    f _ _ = error "no ghc, I'm pretty sure this is total?"

paranoidIntraStrategy :: Int -> System -> (LineId, Int) -> Strategy
paranoidIntraStrategy segSize = f
  where
    center = segSize `div` 2
    f SystemB   (_, i) | i == center = IdentityPerm
    f SystemA q@(_, i) | i == center = DotAgainst SystemB q
    f sys (h, i) | center < i  = DotAgainst sys (h, i-1)
    f sys (h, i) | i < center  = DotAgainst sys (h, i+1)
    f _ _ = error "no ghc, I'm pretty sure this is total?"

type Summaries = (DegeneracyScanSummary, DotSearchSummary, DotAssignSummary)
type Compute a = forall io. (MonadIO io)=> WriterT Summaries io a
computePermAccordingToStrategy :: Uncrosser -> System -> (LineId, Int) -> Strategy -> Compute Perm
computePermAccordingToStrategy u s q strat = do
    liftIO $ IO.putStrLn $ "At " ++ show (s,q) ++ ": Trying Strategy " ++ show strat
    computePermAccordingToStrategy' u s q strat

computePermAccordingToStrategy' :: Uncrosser -> System -> (LineId, Int) -> Strategy -> Compute Perm
computePermAccordingToStrategy' u _ _ IdentityPerm = pure $ Vector.fromList [0..uncrosserNBands u - 1]

computePermAccordingToStrategy' u ketS ketQ (DotAgainst braS braQ) = tryIt
  where
    tryIt = do
        [kets]  <- needOriginalVectors u ketS [ketQ]
        [ketEs] <- needOriginalEnergies u ketS [ketQ]
        [bras]  <- needUncrossedVectors u braS [braQ]
        --liftIO.traceIO $ "ready to dot: " ++ show (ketS, ketQ) ++ " :: " ++ show (braS, braQ)

        let (summaries@(degenSummary, searchSummary, assignSummary), maybeSolution) =
                -- dotAgainstForPerm defaultMatchTolerances bras ketEs kets
                dotAgainstForPerm' defaultMatchTolerances bras kets -- XXX

        liftIO $ IO.putStrLn $ "Summary for " ++ show ((ketS, ketQ), (braS, braQ)) ++ "   " ++
               show ( uncrosserQPath u `qPathAt` ketQ
                    , uncrosserQPath u `qPathAt` braQ
                    )
        showDegenSummary  degenSummary
        showSearchSummary searchSummary
        showAssignSummary assignSummary
        tell summaries

        maybe giveUp pure maybeSolution

    giveUp = do
        liftIO $ traceIO "dotting for perm failed"
        computePermAccordingToStrategy' u ketS ketQ IdentityPerm


-- If we did `needPermutation` or `needUncrossed{Energies,Vectors}` at this point,
--   would we create a dependency cycle?
uncrosserWouldCreateCycle  ::  Uncrosser -> System -> (LineId, Int) -> Compute Bool
uncrosserWouldCreateCycle u s q =
    let ref = uncrosserPermCacheRef u s q
    in liftIO $ (BeingComputed ==) <$> readIORef ref

-----------------------------------------------------------------
-- requesting data at kpoints

-- This is a high-level interface over the caches and the oracle, to be used by the strategies

needOriginalEnergies ::  Uncrosser -> System -> [(LineId, Int)] -> Compute [Energies]
needOriginalEnergies u s = pure . map ((uncrosserOriginalEnergies u s) `qPathAt`)

-- CAUTION: Don't use with too many vectors at once!
needOriginalVectors ::  Uncrosser -> System -> [(LineId, Int)] -> Compute [Kets]
needOriginalVectors u s = liftIO . mapM ((uncrosserOriginalVectors u s) `qPathAt`)

needAllPermutations ::  Uncrosser -> System -> Compute [Perm]
needAllPermutations u s = needPermutations u s $ uncrosserKIds u

needPermutations :: Uncrosser -> System -> [(LineId, Int)] -> Compute [Perm]
needPermutations = permutationCacheRequest

-- CAUTION: Don't use with too many vectors at once!
needUncrossedVectors ::  Uncrosser -> System -> [(LineId, Int)] -> Compute [Kets]
needUncrossedVectors u s q = zipWith Vector.backpermute
                             <$> needOriginalVectors u s q
                             <*> needPermutations u s q

needUncrossedEnergies ::  Uncrosser -> System -> [(LineId, Int)] -> Compute [Energies]
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
permutationCacheRequest :: Uncrosser -> System -> [(LineId, Int)] -> Compute [Perm]
permutationCacheRequest u s q = permutationCacheEnsure u s q
                                >> mapM (permutationCacheGetSingle u s) q

-- get something that is already in the cache
permutationCacheGetSingle :: Uncrosser -> System -> (LineId, Int) -> Compute Perm
permutationCacheGetSingle u s q =
    let ref = uncrosserPermCacheRef u s q
    in liftIO $ readIORef ref >>=
      \case
        Done x -> pure x
        _      -> error "permutationLookupSingle: buggy bug! (not computed!)"

-- make sure things are in the cache.
-- We work on them one-by-one, as some of the things may depend on each other.
permutationCacheEnsure :: Uncrosser -> System -> [(LineId, Int)] -> Compute ()
permutationCacheEnsure u s = mapM_ (permutationCacheEnsureSingle u s)

permutationCacheEnsureSingle :: Uncrosser -> System -> (LineId, Int) -> Compute ()
permutationCacheEnsureSingle u s q =
    let ref = uncrosserPermCacheRef u s q
    in liftIO (readIORef ref) >>=
      \case
        NotStarted -> do
            liftIO $ writeIORef ref BeingComputed
            val <- computeSinglePermutation u s q
            liftIO $ writeIORef ref (Done $!! val)

        BeingComputed -> error "permutationCacheEnsure: buggy bug! (dependency cycle in strategies)"
        Done _ -> pure ()

-- perform the (expensive) computation,
-- descending further down the dependency dag.
computeSinglePermutation :: Uncrosser -> System -> (LineId, Int) -> Compute Perm
computeSinglePermutation u s q = computePermAccordingToStrategy u s q
                                 $ uncrosserStrategy u s q

-----------------------------------------------------------------
-- ket linalg

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

    -- find optimal assignments (maximum total probability) via the hungarian algorithm.
    n = length allBras
    zeros = Vector.fromListN (n*n) [0,0..]
    flatMatrixVec = zeros Vector.// [ (b*n+k, d) | (BraId b, KetLikeId s, d) <- allDots
                                                 , KetId k <- toList $ subspaceIndices (allKetSpaces !! s)
                                                 ]
    (pairs, _) = hungarian (negate <$> toList flatMatrixVec) n n -- negate to maximize
    assignSummary = DotAssignSummary [ flatMatrixVec ! (b*n+k) | (b,k) <- pairs ]
    resultKets = Just $ snd <$> List.sortOn fst pairs

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
        Cli.setSGR [Cli.SetConsoleIntensity Cli.BoldIntensity]
        Cli.setSGR [Cli.SetColor Cli.Foreground Cli.Vivid Cli.Yellow]
        putStr "Number of nontransitive degeneracies: "
        let color = case degeneracyScanNontransitiveCount of 0 -> Cli.Cyan
                                                             _ -> Cli.Red

        Cli.setSGR [Cli.SetConsoleIntensity Cli.BoldIntensity]
        Cli.setSGR [Cli.SetColor Cli.Foreground Cli.Vivid color]
        putStr . show . getSum $ degeneracyScanNontransitiveCount
        putStr $ "/"
        putStr . show . getSum $ degeneracyScanTotalCount
        Cli.setSGR [Cli.Reset]
        putStrLn ""

showSearchSummary :: (MonadIO io)=> DotSearchSummary -> io ()
showSearchSummary summary@DotSearchSummary{..} = liftIO $ doit
  where
    doit = do
        Cli.setSGR [Cli.SetConsoleIntensity Cli.BoldIntensity]
        Cli.setSGR [Cli.SetColor Cli.Foreground Cli.Vivid Cli.Yellow]
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
        Cli.setSGR [Cli.SetConsoleIntensity Cli.BoldIntensity]
        Cli.setSGR [Cli.SetColor Cli.Foreground Cli.Vivid Cli.Yellow]
        putStr "Assignment Quality:"
        Cli.setSGR [Cli.Reset]
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
    MatchTolerances { matchTolDegenerateMaxDiff    = 1e-5
                    , matchTolDotMatrixMinProb     = 1e-5
                    , matchTolNonDegenerateMinDiff = 1e-3
                    , matchTolMaxGuessError        = 1e-2
                    }

-- From a hermitian matrix, get eigenvalues and eigenvectors in an obvious format,
--  sorted ascendingly by eigenvalue.
eigSH :: [[Complex Double]] -> [(Double, [Complex Double])]
eigSH = reverse -- Matrix.eigSH sorts descendingly by eigenvalue
      . uncurry zip
      . (Matrix.toList *** toCols) -- Matrix.eigSH gives vectors as columns
      . Matrix.eigSH . checkedSym . fromRows
  where
    -- hmatrix.toColumns gives storable vectors, a.k.a bfffghgjsfhkeg
    toCols :: (_)=> Matrix a -> [[a]]
    toCols = Matrix.toLists . Matrix.tr'
    fromRows :: (_)=> [[a]] -> Matrix a
    fromRows = Matrix.fromLists

-- A very paranoid constructor for Herm which would rather fail if the input is hermitian
-- than to quietly sweep it under the rug.
checkedSym :: Matrix (Complex Double) -> Matrix.Herm (Complex Double)
checkedSym m = reallyAssert (diffNorm * 1e8 <= origNorm) theSym
  where
    theSym = Matrix.sym m
    m' = Matrix.unSym theSym
    origNorm = sum . fmap sqMagnitude . concat . Matrix.toLists $ m
    diffNorm = sum . fmap sqMagnitude . concat . Matrix.toLists $ m' - m
