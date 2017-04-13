{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

module Band where

import           "base" Data.Foldable
import           "base" Data.Complex
import           "base" System.IO
import           "base" Control.Applicative
import           "base" Control.Monad
import           "base" Control.Monad.IO.Class
import           "base" Control.Arrow
import           "base" Data.IORef
import           "base" Debug.Trace
import           "base" Data.Function((&))
import           "base" Data.Ord (comparing)
import           "base" System.Exit(exitWith, ExitCode(..))
import qualified "base" Data.List as List
import           "containers" Data.Set(Set)
import qualified "containers" Data.Set as Set
import           "containers" Data.Map(Map)
import qualified "containers" Data.Map as Map
import qualified "aeson" Data.Aeson as Aeson
import qualified "yaml" Data.Yaml as Yaml
import qualified "bytestring" Data.ByteString.Lazy as ByteString
import qualified "bytestring" Data.ByteString as ByteString.ByWhichIMeanTheOtherByteString
import qualified "hmatrix" Numeric.LinearAlgebra as Matrix
import           "filepath" System.FilePath((</>))
import           "directory" System.Directory
import           "process" System.Process

-- vector-algorithms needs Unboxed, but those aren't functors, so pbbbbbt
import           "vector" Data.Vector(Vector,(!))
import qualified "vector" Data.Vector as Vector
import           "vector" Data.Vector.Unboxed(Unbox)
import qualified "vector" Data.Vector.Unboxed as Unboxed
import qualified "vector-algorithms" Data.Vector.Algorithms.Intro as Unboxed(sortBy)

import           Band.BandYaml(BandYaml(..))
import qualified Band.BandYaml as BandYaml

-------------------

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

    -- Get these ahead of time
    let vecsA = vecsToPrecompute >>= \case (SystemA, q) -> [q]; _ -> []
    let vecsB = vecsToPrecompute >>= \case (SystemB, q) -> [q]; _ -> []
    _ <- needOriginalVectors uncrosser SystemA vecsA
    _ <- needOriginalVectors uncrosser SystemB vecsB

    --
    permutationsA <- Vector.fromList <$> needAllPermutations uncrosser SystemA
    permutationsB <- Vector.fromList <$> needAllPermutations uncrosser SystemB

    askToWriteCorrectedFile cfgOracleA permutationsA
    askToWriteCorrectedFile cfgOracleB permutationsB

initUncross :: UncrossConfig -> IO Uncrosser
initUncross UncrossConfig{..} = do
    createDirectoryIfMissing True cfgWorkDir
    let uncrosserWorkDir = cfgWorkDir

    -- forM_ [ (systemDirName SystemA, inputFilesetA)
    --       , (systemDirName SystemB, inputFilesetB) ]
    --   $ \(dest, InputFileSet{..}) -> do
    --     dest <- pure $ workDir </> dest
    --     createDirectoryIfMissing True dest
    --     copyFile inputBandJson       $ dest </> "band.json"
    --     copyFile inputPoscar         $ dest </> "POSCAR"
    --     copyFile inputForceSets      $ dest </> "FORCE_SETS"
    --     copyFile inputForceConstants $ dest </> "force_constants.hdf5"
    -- withCurrentDirectory workDir $ runInWorkDir
    let uncrosserOracle = \case SystemA -> cfgOracleA
                                SystemB -> cfgOracleB

    [refsA, refsB] <- forM [SystemA, SystemB] $ \s ->
                        let o = uncrosserOracle s
                        in Vector.forM (Vector.fromList $ oracleLineIds o) $ \h ->
                            let n = oracleLineLength o h
                            in Vector.sequence (Vector.replicate n $ newIORef NotStarted)

    let uncrosserPermCacheRef SystemA ((LineId h), i) = refsA ! h ! i
        uncrosserPermCacheRef SystemB ((LineId h), i) = refsB ! h ! i

    let uncrosserNBands = expect "conflicting values for NBands"
                        $ onlyUniqueValue $ oracleNBands <$> [cfgOracleA, cfgOracleB]

    let uncrosserStrategy s q@(h,_) = defaultStrategy (oracleLineLength (uncrosserOracle s) h) s q

    pure $ Uncrosser
            { uncrosserWorkDir
            , uncrosserNBands
            , uncrosserStrategy
            , uncrosserPermCacheRef
            , uncrosserOracle
            }

-------------------

data System = SystemA -- ^ The eigensystem we're generally trying to match against.
            | SystemB -- ^ The eigensystem we're generally trying to permute.
            deriving (Eq, Show, Read, Ord)

type Ket = Vector (Complex Double)
type Kets = Vector Ket
type Perm = Vector Int
type Energies = Vector Double

data Strategy = Identity
              | DotAgainst !System !(LineId, Int)
              | Extrapolate !Int [Int]
              deriving (Eq, Show, Ord, Read)

-----------------------------------------------------------------

data Uncrosser = Uncrosser
  { uncrosserWorkDir :: FilePath
  , uncrosserStrategy :: System -> (LineId, Int) -> Strategy
  , uncrosserNBands :: Int
  , uncrosserOracle :: System -> Oracle
  , uncrosserPermCacheRef :: System -> (LineId, Int) -> IORef (DagNode Perm)
  }

defaultStrategy :: Int -> System -> (LineId, Int) -> Strategy
defaultStrategy segSize = f
  where
    center = segSize `div` 2
    f SystemA (_, i) | i == center  = Identity
    f SystemB (h, i) | i == center  = DotAgainst SystemA (h, center)
    f s (h, i) | abs (center - i) == 1 = DotAgainst s (h, center)
    f _ (_, i) | abs (center - i) == 2 && center < i  = Extrapolate 2 [i-1, i-2, i-3]
    f _ (_, i) | abs (center - i) == 2 && i < center  = Extrapolate 2 [i+1, i+2, i+3]
    f _ (_, i) | abs (center - i) == 3 && center < i  = Extrapolate 3 [i-1, i-2, i-3, i-4, i-5]
    f _ (_, i) | abs (center - i) == 3 && i < center  = Extrapolate 3 [i+1, i+2, i+3, i+4, i+5]
    f _ (_, i) | center < i  = Extrapolate 3 [i-1, i-2, i-3, i-4, i-5, i-6, i-7]
    f _ (_, i) | i < center  = Extrapolate 3 [i+1, i+2, i+3, i+4, i+5, i+6, i+7]
    f _ _ = error "no ghc, I'm pretty sure this is total?"

linspace :: (Fractional a)=> Int -> a -> a -> [a]
linspace n a b = [ (a * realToFrac (n-1-k) + b * realToFrac k) / realToFrac (n-1)
                 | k <- [0..n-1] ]

computePermAccordingToStrategy :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> Strategy -> io Perm
computePermAccordingToStrategy u s q strat = do
    liftIO $ putStrLn $ "At " ++ show (s,q) ++ ": Trying Strategy " ++ show strat
    computePermAccordingToStrategy' u s q strat

computePermAccordingToStrategy' :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> Strategy -> io Perm
computePermAccordingToStrategy' u _ _ Identity = pure $ Vector.fromList [0..uncrosserNBands u - 1]

computePermAccordingToStrategy' u ketS ketQ (DotAgainst braS braQ) = do
    [kets] <- needOriginalVectors u ketS [ketQ]
    [bras] <- needUncrossedVectors u braS [braQ]
    pure $ dotAgainstForPerm bras kets

computePermAccordingToStrategy' u sys thisQ@(line, thisI) (Extrapolate polyOrder otherIs) = do
    otherEsByBand <- Vector.fromList . List.transpose . fmap toList
                     <$> needUncrossedEnergies u sys ((line,) <$> otherIs)
    [theseEs] <- needOriginalEnergies u sys [thisQ]
    let guessEs = fmap (\otherEs -> extrapolate polyOrder otherIs otherEs thisI) otherEsByBand

    case matchEnergiesForPerm guessEs theseEs of
        Left err -> do
            warn $ "Troublesome energies at " ++ show (sys, thisQ) ++ ": (" ++ err ++ ")"

            -- try dotting against the other system (this could create a cycle)
            -- (fallback: try dotting against one of the points we already requested in this strategy)
            let sys' = uncrosserOtherSystem u sys
            let nearQ = (line, nearest thisI otherIs)
            join $ boolM (computePermAccordingToStrategy u sys thisQ (DotAgainst sys' thisQ))
                         (computePermAccordingToStrategy u sys thisQ (DotAgainst sys nearQ))
                         (uncrosserWouldCreateCycle u sys' thisQ)
            -- computePermAccordingToStrategy u sys thisQ (DotAgainst sys (line, nearest thisI otherIs))
        Right x -> pure x

-- Collect points that will absolutely require eigenvectors at some point, so we can collect them in advance.
uncrosserRequiredEigenvectors :: Uncrosser -> [(System, (LineId, Int))]
uncrosserRequiredEigenvectors u = [ strategyRequiredEigenvectors s q $ uncrosserStrategy u s q
                                  | s <- uncrosserAllSystems u
                                  , q <- uncrosserSystemKIds u s
                                  ] >>= id

-- Collect points that will absolutely require eigenvectors at some point, so we can collect them in advance.
strategyRequiredEigenvectors :: System -> (LineId, Int) -> Strategy -> [(System, (LineId, Int))]
strategyRequiredEigenvectors ketS ketQ (DotAgainst braS braQ) = [(ketS,ketQ), (braS,braQ)]
strategyRequiredEigenvectors _ _ _ = []

warn :: (MonadIO io)=> String -> io ()
warn = liftIO . hPutStrLn stderr

nearest :: (Num a, Ord a, Foldable t)=> a -> t a -> a
nearest x = minimumBy (comparing (abs . subtract x)) . toList

uncrosserAllSystems :: Uncrosser -> [System]
uncrosserAllSystems = const [SystemA, SystemB]

uncrosserOtherSystem :: Uncrosser -> System -> System
uncrosserOtherSystem _ SystemA = SystemB
uncrosserOtherSystem _ SystemB = SystemA

uncrosserSystemKIds :: Uncrosser -> System -> [(LineId, Int)]
uncrosserSystemKIds u s = oracleKIds $ uncrosserOracle u s

-----------------------------------------------------------------
-- requesting kpoints

type KPoint = [Double]

needOriginalEnergies :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Energies]
needOriginalEnergies u s = liftIO . askEigenvalues (uncrosserOracle u s)

needOriginalVectors :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Kets]
needOriginalVectors = eigenvectorCacheRequest

needAllPermutations :: (MonadIO io)=> Uncrosser -> System -> io [Perm]
needAllPermutations u s = needPermutations u s $ uncrosserSystemKIds u s

needPermutations :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Perm]
needPermutations = permutationCacheRequest

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
            warn $ "Finished at " ++ show (s, q)
            writeIORef ref (Done val)

        BeingComputed -> error "permutationCacheEnsure: buggy bug! (dependency cycle in strategies)"
        Done _ -> pure ()

uncrosserWouldCreateCycle  :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Bool
uncrosserWouldCreateCycle u s q =
    let ref = uncrosserPermCacheRef u s q
    in liftIO $ (== BeingComputed) <$> readIORef ref

-- perform the (expensive) computation,
-- descending further down the dependency dag.
computeSinglePermutation :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Perm
computeSinglePermutation u s q = computePermAccordingToStrategy u s q
                                 $ uncrosserStrategy u s q

-----------------------------------------------------------------
-- filesystem-based cache of eigenvectors from phonopy

-- request a bunch of values, which may or may not already be cached
eigenvectorCacheRequest :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Kets]
eigenvectorCacheRequest u s q = eigenvectorCacheEnsure u s q
                                >> mapM (eigenvectorCacheGetSingle u s) q

-- make sure things are in the cache
eigenvectorCacheEnsure :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io ()
eigenvectorCacheEnsure u s q = do
    q <- filterM (fmap not <$> eigenvectorCacheHasSingle u s) q
    if null q then pure ()
    else do
        vecs <- computeEigenvectors u s q
        zipWithM_ (eigenvectorCachePutSingle u s) q vecs

eigenvectorCacheHasSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Bool
eigenvectorCacheHasSingle u s q = eigenvectorCachePath u s q >>= liftIO . doesFileExist

-- get something that is known to be in the cache
eigenvectorCacheGetSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Kets
eigenvectorCacheGetSingle u s q = do
    eigenvectorCachePath u s q
          >>= liftIO . fmap ketsFromCereal . readJson

eigenvectorCachePutSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> Kets -> io ()
eigenvectorCachePutSingle u s q kets = do
    eigenvectorCachePath u s q
          >>= liftIO . flip writeJson (ketsToCereal kets)

eigenvectorCachePath :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io FilePath
eigenvectorCachePath u s q@(LineId h, i) = do
    let dir = uncrosserWorkDir u </> "eigenvecs"
    liftIO $ createDirectoryIfMissing True dir  -- XXX this function ought to be pure...
    pure $ dir </> concat [ show s, "-", show h, "-", show i, ".json" ]

-- perform the (expensive) computation.
-- We can do many points at once to alleviate against phonopy's startup time.
computeEigenvectors :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Kets]
computeEigenvectors u s = liftIO . askEigenvectors (uncrosserOracle u s)

-- part of this balanced breakfast
type KetsCereal = Vector (Vector (Double, Double))

ketsFromCereal :: KetsCereal -> Kets
ketsFromCereal = ffmap (uncurry (:+))

ketsToCereal :: Kets -> KetsCereal
ketsToCereal = ffmap (realPart &&& imagPart)


-----------------------------------------------------------------
-- projection strategy


dotAgainstForPerm :: Kets -- bras (those we're matching against)
                  -> Kets -- kets (those being permuted)
                  -> Perm
dotAgainstForPerm bras kets = Vector.fromList . fmap (\(KetId k) -> k) $ result
  where
    dots = getNonzeroDots 1e-5 bras kets

    -- Because pretty much everything implements Ord, we explicitly annotate the
    -- types of what we're comparing, to help the type checker yell at us more.
    dotsInOrder = snd <$> List.sortBy (comparing (fst :: _ -> BraId)) dots

    -- Greedily pick the largest ket for each bra in order.
    -- This is not necessarily optimal.
    result = rec Set.empty dotsInOrder where
        rec _ [] = []
        rec usedKets (theseDots:moreDots) = chosen : rec usedKets' moreDots where
            chosen = theseDots
                     & filter ((`notElem` usedKets) . snd)
                     & List.maximumBy (comparing (fst :: _ -> Double))
                     & snd
            usedKets' = Set.insert chosen usedKets

newtype BraId = BraId Int deriving (Eq, Show, Ord, Read)
newtype KetId = KetId Int deriving (Eq, Show, Ord, Read)

getNonzeroDots :: Double -> Kets -> Kets -> [(BraId, [(Double, KetId)])]
getNonzeroDots threshold allBras allKets = fmap (braId &&& braNonzeroDots)
                                         $ loopOverKets [] initialBras initialKets
  where
    initialBras = zipWith (BraState 1 True []) (BraId <$> [0..]) (toList allBras)
    initialKets = zip (KetId <$> [0..]) (toList allKets)

    loopOverKets out bras [] = out ++ bras
    loopOverKets out bras (ket:kets) = loopOverKets out' bras' kets
      where
        actions = getBraActions (scanRowWithKet ket bras)
        -- Note that because the ShortCircuit is the final action whenever present,
        -- it contributes O(1) cost to the following concatenation.
        bras' = actions >>= toKeepFromAction
        out'  = (actions >>= toFinishFromAction) ++ out

    scanRowWithKet :: (KetId, Ket) -> [BraState] -> [BraState]
    scanRowWithKet ket = rec 1 where
        rec _ [] = []
        rec remainProb bras@(bra:rest)
                | remainProb <= threshold = bras -- short-circuit to leave many bras pristine
                | otherwise               = let (prob, bra') = braKetUpdate ket bra
                                            in bra' : rec (remainProb - prob) rest

    -- Inspect a matrix element, performing a dot product and updating a bra's state.
    braKetUpdate :: (KetId, Ket) -> BraState -> (Double, BraState)
    braKetUpdate (ketI, ket) (BraState remainingProb _ dotProds braId bra)
        = (thisProb, BraState remainingProb' False dotProds' braId bra)
      where
        thisProb       = sqMagnitude (bra `ketDot` ket)
        remainingProb' = remainingProb - thisProb
        dotProds'      = (thisProb, ketI) : dotProds

    -- Filter out bras we no longer need to track, thus trimming the left edge of the
    --   region of the matrix that we are searching.
    getBraActions :: [BraState] -> [BraAction]
    getBraActions = rec where
        rec [] = []
        -- short-circuiting base case dodges accidental O(n^2) filtering.
        -- Notice that it must be the final item produced, so that it contributes
        --  O(1) total cost during the `toKeepFromAction` flat-map operation.
        rec bras@(bra:_) | braIsPristine bra = [ShortCircuit bras]
        rec (bra:rest) | braStillUseful bra = KeepTracking bra : rec rest
                       | otherwise          = FinishBra    bra : rec rest

    -- It is tempting to stop searching for a bra's partner once the best probability
    --  exceeds the total remaining probability;  but doing so in turn makes it difficult
    --  to know when we're done *with a ket.* Look strictly at total probability instead.
    braStillUseful :: BraState -> Bool
    braStillUseful = (threshold <=) . braUnusedProb

    toKeepFromAction (KeepTracking bra) = [bra]
    toKeepFromAction (FinishBra _)      = []
    toKeepFromAction (ShortCircuit bras) = bras

    toFinishFromAction (FinishBra bra)  = [bra]
    toFinishFromAction _ = []

data BraAction = KeepTracking BraState
               | FinishBra    BraState
               | ShortCircuit [BraState]


data BraState = BraState
    { braUnusedProb  :: Double            -- remaining probability not yet accounted for
    , braIsPristine  :: Bool              -- have we attempted to dot this with at least one ket?
    , braNonzeroDots :: [(Double, KetId)] -- (prob, ketId) for kets with nonzero overlap
    , braId          :: BraId             -- the bra index
    , braBra         :: Ket               -- the bra!
    }



-- ------------------------------xxxxxxxxxxxxxx
-- -- Use dot products to produce a permutation that permutes the kets
-- -- to match the bras.
-- -- NOTE: Unchecked preconditions:
-- --  * All bras/kets are normalized to a self-inner-product of 1.
-- dotAgainstForPermOld :: Kets -- bras (those we're matching against)
--                   -> Kets -- kets (those being permuted)
--                   -> Perm
-- dotAgainstForPermOld a b = Vector.fromList $ rec [] (toList a) (zip [0..] $ toList b)
--   where
--     rec _ [] [] = []
--     rec usedKets (bra:bras') kets = i : rec usedKets'' bras' kets'
--       where
--         (initProb, usedKets') = applyUsedKets bra usedKets
--         (ketProb, (i,ket), kets') = popClosestKet initProb bra kets
--         usedKets'' = (1-ketProb, ket) : pruneUsedKets usedKets'
--     rec _ _ _ = error "dotAgainstForPerm: unequal number of bras and kets!"

-- -- dot a bra against all previously used kets that are still being tracked
-- applyUsedKets :: Ket                      -- this bra
--               -> [(Double,Ket)]           -- used kets and their used probabilities
--               -> (Double, [(Double,Ket)]) -- remaining prob of this bra and used kets
-- applyUsedKets bra kets = (outProb, kets')
--   where
--     ketProbs = fmap (sqMagnitude . ketDot bra . snd) kets
--     outProb = 1 - sum ketProbs
--     kets' = zipWith (\(p,ket) q -> (p - q, ket)) kets ketProbs

-- -- stop tracking kets that have nothing left to give
-- pruneUsedKets :: _ => [(Double,Ket)] -> [(Double, Ket)]
-- pruneUsedKets = filter ((1e-4 <) . fst)

-- -- oops
-- popClosestKet :: (Eq s, Show s)=> Double -> Ket -> [(s, Ket)] -> (Double, (s, Ket), [(s, Ket)])
-- popClosestKet initProb = flip (popClosestBra initProb)

-- -- FIXME make this into popClosestKet proper
-- -- NOTE: Unchecked preconditions:
-- --  * The 's' values uniquely label each bra. (none appear twice)
-- popClosestBra :: (Eq s,Show s)
--               => Double                 -- total remaining probability of this ket
--               -> [(s, Ket)]             -- unused bras, labeled
--               -> Ket                    -- this ket
--               -> (Double, (s, Ket), [(s, Ket)]) -- closest bra, its prob with this ket, and the list with it removed
-- popClosestBra _ _ _  | trace "popClosestBra" False = undefined
-- popClosestBra _ [] _   = error "popClosestBra: empty list"
-- popClosestBra initProb allBras ket = rec initProb 0 (error "popClosestBra: buggy bug!") allBras
--   where
--     rec  r b _ (this:_) | traceShow (fst this, r, b, snd this `ketDot` ket) False = undefined
--     rec _ _ _ [] = error "popClosestBra: something strange happened (vectors not normalized?)"
--     rec remainingProb bestProb bestBra (thisBra:bras)
--         | remainingProb < bestProb = (bestProb, bestBra, otherBras bestBra)
--         | thisProb > bestProb = rec remainingProb' thisProb thisBra bras
--         | otherwise           = rec remainingProb' bestProb bestBra bras
--       where
--         thisProb = sqMagnitude (snd thisBra `ketDot` ket)
--         remainingProb' = remainingProb - thisProb
--         otherBras bra = findDelete ((fst bra ==) . fst) allBras

------------------------------xxxxxxxxxxxxxx

-- Good thing the stdlib provided us with the extremely fundamental 'deleteBy', which
-- generalizes over, uhm... wait, equality comparators!? That can't be right...
-- hm.....yep, that's all they gave us.
findDelete :: (a -> Bool) -> [a] -> [a]
findDelete _ []     = []
findDelete f (x:xs) = if f x then xs else x : findDelete f xs

sqMagnitude :: (Num a)=> Complex a -> a
sqMagnitude (a :+ b) = a*a + b*b

ketDot :: Ket -> Ket -> Complex Double
ketDot a b = sum $ Vector.zipWith (*) (conjugate <$> a) b

-----------------------------------------------------------------
-- extrapolation strategy

data MatchTolerances a =
  MatchTolerances { matchTolDegenerateMaxDiff :: a    -- ^ If two bands are within (<=) this amount of energy
                                                      --   they are considered degenerate.
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
                    , matchTolNonDegenerateMinDiff = 1e-9
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
sortArgSort xs = (Vector.convert *** Vector.convert) . Unboxed.unzip $ Unboxed.create $ do
    -- Don't change the first '$' above to a '.', you'll kill type inference.
    -- the ugly monad machinery is for sortBy.
    xsi <- Unboxed.thaw . Unboxed.indexed . Vector.convert $ xs
    Unboxed.sortBy (comparing snd) xsi
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
solveLinearSystemLS a b = Matrix.toList . Matrix.flatten
                          $ Matrix.linearSolveLS (Matrix.fromRows . fmap Matrix.fromList $ a)
                                                 (Matrix.asColumn . Matrix.fromList $ b)

-----------------------------------------------------------------
-- The oracle. It knows about the problem we are trying to solve.
-- (where does the eigensystem come from? What do we want to produce in the end?)

data Oracle = Oracle
  { oracleRootDir :: FilePath
  , oracleNBands :: Int
  , oracleHSymPoints :: Map HSymPoint KPoint
  , oracleHSymPath :: Vector HSymPoint
  , oracleLineLengths_ :: Vector Int
  , oraclePoints_ :: Vector (Vector KPoint)
  , oracleEnergies_ :: Vector (Vector Energies)
  }

data OracleLineData = OracleLineData
  { segmentPoints :: Vector KPoint       -- ^ The reciprocal-space coords of each point.
  , segmentOriginalEs :: Vector Energies -- ^ Band energies at each point, in original order.
  }


initOracle :: FilePath -> IO Oracle
initOracle oracleRootDir =
  withCurrentDirectory oracleRootDir $ do

    let expectFile s = doesPathExist s >>= bool (fail $ "expected file: " ++ s) (pure ())

    BandJsonParseData{..} <- readYaml "eigenvalues.yaml"
    HighSymInfo{..} <- readJson "hsym.json"
    expectFile "oracle.conf"
    expectFile "POSCAR" -- phonopy wants this for "symmetry"...
    expectFile "FORCE_SETS"

    let oracleNBands = 3 * bandJsonNAtoms
    let oracleLineLengths_ = bandJsonLineLengths
    let oracleHSymPoints = highSymInfoPoints
    let oracleHSymPath = highSymInfoPath

    let pathKPoints = (oracleHSymPoints Map.!) <$> oracleHSymPath
    let oraclePoints_ = Vector.zipWith3 kpointLinspace oracleLineLengths_
                                                       (Vector.init pathKPoints)
                                                       (Vector.tail pathKPoints)

    let oracleEnergies_ = Vector.fromList $ partitionVector (toList bandJsonLineLengths)
                                                            bandJsonEnergies

    pure $ Oracle
        { oracleRootDir
        , oracleNBands
        , oracleLineLengths_
        , oracleEnergies_
        , oracleHSymPoints
        , oracleHSymPath
        , oraclePoints_
        }

askEigenvalues :: Oracle -> [(LineId, Int)] -> IO [Energies]
askEigenvalues o qs = pure $
    flip map qs $ \(LineId h, i) ->
        oracleEnergies_ o ! h ! i

askEigenvectors :: Oracle -> [(LineId, Int)] -> IO [Kets]
askEigenvectors o qs =
  withCurrentDirectory (oracleRootDir o) $ do
    let bandStrParts :: [Double]
        bandStrParts = (qs >>= \(LineId h,i) -> oraclePoints_ o ! h ! i) -- o hi there
                       ++ [0,0,0] -- one extra point for the "end" of the last segment
    let bandStr = List.intercalate " " (show <$> bandStrParts)

    callProcess "phonopy" ["oracle.conf", "--readfc", "--eigenvectors", "--band_points=1", "--band=" ++ bandStr]

    vecs <- fromEigenvectorParseData <$> readYaml "band.yaml"
    removeFile "band.yaml"
    pure . toList $ vecs

askToWriteCorrectedFile :: Oracle -> Vector Perm -> IO ()
askToWriteCorrectedFile o perms =
  withCurrentDirectory (oracleRootDir o) $ do
    (permuteBandYaml perms <$> readYaml "eigenvalues.yaml") >>= writeYaml "corrected.yaml"

readJsonEither :: (Aeson.FromJSON a)=> FilePath -> IO (Either String a)
readJsonEither = ByteString.readFile >=> pure . Aeson.eitherDecode

readJson :: (Aeson.FromJSON a)=> FilePath -> IO a
readJson = readJsonEither >=> either fail pure

writeJson :: (Aeson.ToJSON a)=> FilePath -> a -> IO ()
writeJson p = Aeson.encode >>> ByteString.writeFile p

readYaml :: (Aeson.FromJSON a)=> FilePath -> IO a
readYaml = ByteString.ByWhichIMeanTheOtherByteString.readFile >=> Yaml.decodeEither >>> either fail pure

writeYaml :: (Aeson.ToJSON a)=> FilePath -> a -> IO ()
writeYaml = Yaml.encodeFile -- how nice of them!

partitionVector :: [Int] -> Vector a -> [Vector a]
partitionVector [] v | null v = []
                     | otherwise = error "partitionVector: Vector longer than total output length"
partitionVector (n:ns) v | n > length v = error "partitionVector: Vector shorter than total output length"
                         | otherwise    = Vector.take n v : partitionVector ns (Vector.drop n v)

data BandJsonParseData = BandJsonParseData
  { bandJsonNAtoms :: Int
  , bandJsonLineLengths :: Vector Int
  , bandJsonEnergies :: Vector Energies
  }

data HighSymInfo = HighSymInfo
  { highSymInfoPoints :: Map HSymPoint KPoint
  , highSymInfoPath :: Vector HSymPoint
  }

instance Aeson.FromJSON HighSymInfo where
    parseJSON = Aeson.withObject "highsym info" $ \o ->
        HighSymInfo <$> o Aeson..: "point" <*> o Aeson..: "path"

newtype EigenvectorParseData = EigenvectorParseData
    { fromEigenvectorParseData :: Vector (Vector (Vector (Complex Double))) }

instance Aeson.FromJSON BandJsonParseData where
    parseJSON = Aeson.parseJSON >>> fmap postprocess where

        postprocess BandYaml{..} = BandJsonParseData{..} where
            bandJsonLineLengths = Vector.fromList bandYamlSegmentNQPoint
            bandJsonNAtoms = bandYamlNAtom
            bandJsonEnergies = bandYamlSpectrum                 -- :: Vector SpectrumData
                               & fmap BandYaml.spectrumBand     -- :: Vector (Vector DataBand)
                               & ffmap BandYaml.bandFrequency   -- :: Vector (Vector Energies)

instance Aeson.FromJSON EigenvectorParseData where
    parseJSON = Aeson.parseJSON >=> postprocess where

        postprocess BandYaml{..} = ohDear where
          ohDear =                                     -- (read V as Vector. Implicitly nested to the right)
            bandYamlSpectrum                           -- :: V SpectrumData
            & fmap BandYaml.spectrumBand               -- :: V V DataBand
            & ffmap BandYaml.bandEigenvector           -- :: V V Maybe V V (Double ,Double)
            & Vector.mapM (Vector.mapM id)             -- :: Maybe V V V V (Double, Double)
            & maybe (fail "Missing eigenvectors") pure -- :: Parser V V V V (Double, Double)
            & fffmap join  {- 3xN to 3N cartesian -}   -- :: Parser V V V (Double, Double)
            & ffffmap (uncurry (:+))                   -- :: Parser V V V (Complex Double)
            & fmap EigenvectorParseData                -- :: Parser EigenvectorParseData


permuteBandYaml :: Vector Perm -> BandYaml -> BandYaml
permuteBandYaml perms yaml = yaml'
  where
    spectrum = bandYamlSpectrum yaml
    spectrum' = Vector.zipWith permuteBandYamlSpectrumEntry perms spectrum
    yaml' = yaml{bandYamlSpectrum = spectrum'}

permuteBandYamlSpectrumEntry :: Perm -> BandYaml.SpectrumData -> BandYaml.SpectrumData
permuteBandYamlSpectrumEntry perm dat = dat'
  where
    bands = BandYaml.spectrumBand dat
    bands' = bands `Vector.backpermute` perm
    dat' = dat{BandYaml.spectrumBand = bands'}

exitOnFailure :: ExitCode -> IO ()
exitOnFailure ExitSuccess = pure ()
exitOnFailure e = exitWith e

oracleLineIds :: Oracle -> [LineId]
oracleLineIds o = LineId <$> [0..length (oracleHSymPath o) - 2]
oracleLineLength :: Oracle -> LineId -> Int
oracleLineLength o (LineId h) = oracleLineLengths_ o Vector.! h
oracleKIds :: Oracle -> [(LineId, Int)]
oracleKIds o = oracleLineIds o >>= \h -> (h,) <$> [0..oracleLineLength o h - 1]

newtype LineId = LineId Int
                 deriving (Eq, Ord, Show, Read)

data HSymLine = HSymLine
  { hsymLineFrom :: HSymPoint
  , hsymLineTo   :: HSymPoint
  } deriving (Eq, Ord, Show, Read)

type HSymPoint = String

kpointLinspace :: Fractional a => Int -> [a] -> [a] -> Vector [a]
kpointLinspace n kFrom kTo = result
  where
    ZipList segmentPointsZip = linspace n <$> ZipList kFrom <*> ZipList kTo
    result = Vector.fromList $ List.transpose segmentPointsZip



-- helps debug type errors
tc :: () -> ()
tc = id

-- fffffunctions for fffffunctors
ffmap :: (_) => (a -> b) -> s (t a) -> s (t b)
ffmap = fmap . fmap
fffmap :: (_) => (a -> b) -> s (t (u a)) -> s (t (u b))
fffmap = fmap . ffmap
ffffmap :: (_) => (a -> b) -> s (t (u (v a))) -> s (t (u (v b)))
ffffmap = fmap . fffmap

-- folds for validating redundant data
onlyValue :: (Foldable t)=> t a -> Maybe a
onlyValue xs = case toList xs of [x] -> Just x
                                 _   -> Nothing

bool :: a -> a -> Bool -> a
bool a _ False = a
bool _ b True  = b

boolM :: (Functor m)=> a -> a -> m Bool -> m a
boolM a b m = bool a b <$> m

onlyUniqueValue :: (Eq a, Foldable t)=> t a -> Maybe a
onlyUniqueValue = onlyValue . List.nub . toList -- This usage of nub is O(n) in the common case.

expect :: String -> Maybe a -> a
expect msg = maybe (error msg) id
