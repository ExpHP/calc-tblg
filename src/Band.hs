{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           "base" Debug.Trace
import           "base" Data.Foldable
import           "base" Data.Monoid
import           "base" Data.Complex
import           "base" System.IO
import           "base" Control.Applicative
import           "base" Control.Monad
import           "base" Control.Monad.IO.Class
import           "base" Control.Arrow
import           "base" Control.Exception
import           "base" Data.Functor.Identity(runIdentity)
import           "base" Data.IORef
import           "base" Data.Ord (comparing)
import qualified "base" Data.List as List
import           "containers" Data.Map(Map)
import qualified "containers" Data.Map as Map
import           "aeson" Data.Aeson((.=))
import qualified "aeson" Data.Aeson as Aeson
import           "bytestring" Data.ByteString.Lazy(ByteString)
import qualified "bytestring" Data.ByteString.Lazy as ByteString
import           "random" System.Random
import qualified "hmatrix" Numeric.LinearAlgebra as Matrix
import           "filepath" System.FilePath((</>))
import           "directory" System.Directory

-- vector-algorithms needs Unboxed, but those aren't functors, so pbbbbbt
import           "vector" Data.Vector(Vector,(!))
import qualified "vector" Data.Vector as Vector
import           "vector" Data.Vector.Unboxed(Unbox)
import qualified "vector" Data.Vector.Unboxed as Unboxed
import qualified "vector-algorithms" Data.Vector.Algorithms.Intro as Unboxed(sortBy)

-------------------

runUncross :: UncrossConfig -> IO ()
runUncross UncrossConfig{..} = do
    createDirectoryIfMissing True workDir
    forM_ [ (systemDirName SystemA, inputFilesetA)
          , (systemDirName SystemB, inputFilesetB) ]
      $ \(dest, InputFileSet{..}) -> do
        dest <- pure $ workDir </> dest
        createDirectoryIfMissing True dest
        copyFile inputBandJson       $ dest </> "band.json"
        copyFile inputPoscar         $ dest </> "POSCAR"
        copyFile inputForceSets      $ dest </> "FORCE_SETS"
        copyFile inputForceConstants $ dest </> "force_constants.hdf5"

    withCurrentDirectory workDir $ runInWorkDir

-------------------

data OutputFileSet = OutputFileSet
  { outputBandJson :: FilePath
  , outputPerms    :: FilePath
  } deriving (Show, Eq, Ord, Read)

data UncrossConfig = UncrossConfig
  { inputFilesetA :: InputFileSet
  , inputFilesetB :: InputFileSet
  , outputFileset :: OutputFileSet
  , workDir :: FilePath
  } deriving (Show, Eq, Ord, Read)

data InputFileSet = InputFileSet
  { inputBandJson       :: FilePath
  , inputPoscar         :: FilePath
  , inputForceSets      :: FilePath
  , inputForceConstants :: FilePath
  } deriving (Show, Eq, Ord, Read)

data InputFileTag = BandJson FilePath
                  | Poscar FilePath
                  | ForceSets FilePath
                  | ForceConstants FilePath

inputFileset :: FilePath -> [InputFileTag] -> InputFileSet
inputFileset root tags =
    InputFileSet
        (getPath "band.json"            (\case (BandJson x)       -> Just x; _ -> Nothing))
        (getPath "POSCAR"               (\case (Poscar x)         -> Just x; _ -> Nothing))
        (getPath "FORCE_SETS"           (\case (ForceSets x)      -> Just x; _ -> Nothing))
        (getPath "force_constants.hdf5" (\case (ForceConstants x) -> Just x; _ -> Nothing))
  where
    getPath :: FilePath -> (InputFileTag -> Maybe FilePath) -> FilePath
    getPath def projector = (root </>) . maybe def id $ msum (projector <$> tags)

-------------------

runInWorkDir :: IO ()
runInWorkDir = do
    error "x"

-------------------

 -- | Identifies a region with analytic bands.
data Segment = Segment
  { segmentSystem :: System
  , segmentHSymLine :: HSymLine
  } deriving (Eq, Show, Read, Ord)

data HSymLine = HSymLine
  { hsymLineFrom :: HSymPoint
  , hsymLineTo   :: HSymPoint
  } deriving (Eq, Show, Read, Ord)

type HSymPoint = String

data System = SystemA -- ^ The eigensystem we're generally trying to match against.
            | SystemB -- ^ The eigensystem we're generally trying to permute.
            deriving (Eq, Show, Read, Ord)

systemDirName :: System -> FilePath
systemDirName SystemA = "a"
systemDirName SystemB = "b"

type Ket = Vector (Complex Double)
type Kets = Vector Ket
type Perm = Vector Int
type Energies = Vector Double

data Strategy = Identity
              | DotAgainst !Segment !Int
              | Extrapolate [Int]

-----------------------------------------------------------------

defaultStrategy :: Int -> Segment -> Int -> Strategy
defaultStrategy segSize = f
  where
    center = segSize `div` 2
    f (Segment SystemA _)    i | i == center  = Identity
    f (Segment SystemB line) i | i == center  = DotAgainst (Segment SystemA line) center
    f seg i | abs (center - i) == 1           = DotAgainst seg center
    f seg i | center < i  = Extrapolate [i-1, i-2, i-3]
    f seg i | i < center  = Extrapolate [i+1, i+2, i+3]

callerOfDefaultStrategy  = error "TODO: caller?"
callerOfconstructSegment = error "TODO: caller?"

constructSegment :: (MonadIO io)
                 => Map HSymPoint KPoint
                 -> Vector Energies -> Segment -> io OracleSegmentData
constructSegment kmap energies seg@Segment{..} = do
    segmentPermCache <- liftIO $ initSegmentPermCache
    pure $ OracleSegmentData { segmentPoints, segmentStrategies, segmentOriginalEs, segmentPermCache }

  where
    numKPoint = length energies
    center = numKPoint `div` 2

    HSymLine{..} = segmentHSymLine
    kFrom = kmap Map.! hsymLineFrom
    kTo   = kmap Map.! hsymLineTo

    ZipList segmentPointsZip = linspace numKPoint <$> ZipList kFrom <*> ZipList kTo
    segmentPoints = Vector.fromList $ List.transpose segmentPointsZip
    segmentStrategies = defaultStrategy numKPoint seg <$> Vector.fromList [0..numKPoint-1]
    segmentOriginalEs = energies
    initSegmentPermCache = mapM (const $ newIORef NotStarted) energies

linspace :: (Fractional a)=> Int -> a -> a -> [a]
linspace n a b = [ (a * realToFrac (n-1-k) + b * realToFrac k) / realToFrac (n-1)
                 | k <- [0..n-1] ]

computePermAccordingToStrategy :: (MonadIO io)=> Oracle -> Segment -> Int -> Strategy -> io Perm
computePermAccordingToStrategy oracle _ _ Identity = pure $ Vector.fromList [0..oracleNBands oracle - 1]

computePermAccordingToStrategy oracle ketSeg ketI (DotAgainst braSeg braI) = do
    [kets] <- needOriginalVectors oracle ketSeg [ketI]
    [bras] <- needUncrossedVectors oracle braSeg [braI]
    pure $ dotAgainstForPerm bras kets

computePermAccordingToStrategy oracle seg thisI (Extrapolate otherIs) = do
    otherEsByBand <- Vector.fromList . List.transpose . fmap toList
                     <$> needUncrossedEnergies oracle seg otherIs
    [theseEs] <- needOriginalEnergies oracle seg [thisI]
    let guessEs = fmap (\otherEs -> extrapolate otherIs otherEs thisI) otherEsByBand

    case matchEnergiesForPerm guessEs theseEs of
        Left err -> do
            warn $ "Troublesome energies at " ++ show thisI ++ ": (" ++ err ++ ")"
            computePermAccordingToStrategy oracle seg thisI (DotAgainst seg (nearest thisI otherIs))
        Right x -> pure x

warn :: (MonadIO io)=> String -> io ()
warn = liftIO . hPutStrLn stderr

nearest :: (Num a, Ord a, Foldable t)=> a -> t a -> a
nearest x = minimumBy (comparing (abs . subtract x)) . toList

-----------------------------------------------------------------
-- requesting kpoints

type KPoint = [Double]
data Oracle = Oracle
  { oracleNBands :: Int
  , oracleHSymPoints :: Map HSymPoint KPoint
  , oracleHSymPath :: [HSymPoint]
  , oracleSegmentDataMap :: Map Segment OracleSegmentData
  }

oracleSegmentData :: Oracle -> Segment -> OracleSegmentData
oracleSegmentData oracle seg = expect "oracleSegmentData: bad segment"
                               . Map.lookup seg $ oracleSegmentDataMap oracle

data OracleSegmentData = OracleSegmentData
  { segmentPoints :: Vector KPoint       -- ^ The reciprocal-space coords of each point.
  , segmentOriginalEs :: Vector Energies -- ^ Band energies at each point, in original order.
  , segmentStrategies :: Vector Strategy -- ^ The permutation strategies to be used at each kpoint.

    -- | The possibly-not-yet determined permutation at each point
    --   which untwists the bands when applied to the corresponding
    --   element of segmentOriginalEs with `Vector.backpermute`.
  , segmentPermCache :: Vector (IORef (DagNode Perm))
  }

segmentNPoints :: OracleSegmentData -> Int
segmentNPoints = length . segmentOriginalEs

needOriginalEnergies :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io [Energies]
needOriginalEnergies o s = pure . fmap (segmentOriginalEs (oracleSegmentData o s) Vector.!)

needOriginalVectors :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io [Kets]
needOriginalVectors = eigenvectorCacheRequest

needPermutations :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io [Perm]
needPermutations = permutationCacheRequest

needUncrossedVectors :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io [Kets]
needUncrossedVectors o s i = zipWith Vector.backpermute
                             <$> needOriginalVectors o s i
                             <*> needPermutations o s i

needUncrossedEnergies :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io [Energies]
needUncrossedEnergies o s i = zipWith Vector.backpermute
                              <$> needOriginalEnergies o s i
                              <*> needPermutations o s i

-----------------------------------------------------------------
-- permutation computation, strongly tied to a cache.
--
-- The Strategies implicitly define a data dependency dag.

data DagNode a = NotStarted    -- first time seeing node (tree edge)
               | BeingComputed -- currently in the path traveled from the root (back edge!)
               | Done a        -- node already computed (forward/cross edge)
               deriving (Eq, Show, Read, Ord)

-- request a bunch of values, which may or may not already be cached
permutationCacheRequest :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io [Perm]
permutationCacheRequest o s i = permutationCacheEnsure o s i
                                >> mapM (permutationCacheGetSingle o s) i

-- get something that is already in the cache
permutationCacheGetSingle :: (MonadIO io)=> Oracle -> Segment -> Int -> io Perm
permutationCacheGetSingle o s i =
    let ref = segmentPermCache (oracleSegmentData o s) Vector.! i
    in liftIO $ readIORef ref >>=
      \case
        Done x -> pure x
        _      -> error "permutationLookupSingle: buggy bug! (not computed!)"

-- make sure things are in the cache.
-- We work on them one-by-one, as some of the things may depend on each other.
permutationCacheEnsure :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io ()
permutationCacheEnsure o s = mapM_ (permutationCacheEnsureSingle o s)

permutationCacheEnsureSingle :: (MonadIO io)=> Oracle -> Segment -> Int -> io ()
permutationCacheEnsureSingle o s i =
    let ref = segmentPermCache (oracleSegmentData o s) Vector.! i
    in liftIO $ readIORef ref >>=
      \case
        NotStarted -> do
            writeIORef ref BeingComputed
            val <- computeSinglePermutation o s i
            writeIORef ref (Done val)

        BeingComputed -> error "permutationCacheEnsure: buggy bug! (dependency cycle in strategies)"
        Done _ -> pure ()

-- perform the (expensive) computation,
-- descending further down the dependency dag.
computeSinglePermutation :: (MonadIO io)=> Oracle -> Segment -> Int -> io Perm
computeSinglePermutation o s i = computePermAccordingToStrategy o s i
                                 . (Vector.! i) . segmentStrategies
                                 $ oracleSegmentData o s

-----------------------------------------------------------------
-- filesystem-based cache of eigenvectors from phonopy

-- request a bunch of values, which may or may not already be cached
eigenvectorCacheRequest :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io [Kets]
eigenvectorCacheRequest o s i = eigenvectorCacheEnsure o s i
                                >> mapM (eigenvectorCacheGetSingle o s) i

-- make sure things are in the cache
eigenvectorCacheEnsure :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io ()
eigenvectorCacheEnsure o s idx = do
    idx <- filterM (eigenvectorCacheHasSingle o s) idx
    vecs <- computeEigenvectors o s idx
    zipWithM_ (eigenvectorCachePutSingle o s) idx vecs

eigenvectorCacheHasSingle :: (MonadIO io)=> Oracle -> Segment -> Int -> io Bool
eigenvectorCacheHasSingle = error "TODO: query filesystem"

-- get something that is known to be in the cache
eigenvectorCacheGetSingle :: (MonadIO io)=> Oracle -> Segment -> Int -> io Kets
eigenvectorCacheGetSingle = error "TODO: read from filesystem"

eigenvectorCachePutSingle :: (MonadIO io)=> Oracle -> Segment -> Int -> Kets -> io ()
eigenvectorCachePutSingle = error "TODO: query filesystem"

-- perform the (expensive) computation.
-- We can do many points at once to alleviate against phonopy's startup time.
computeEigenvectors :: (MonadIO io)=> Oracle -> Segment -> [Int] -> io [Kets]
computeEigenvectors = error "TODO: call phonopy"

-----------------------------------------------------------------
-- projection strategy

dotAgainstForPerm :: Kets -> Kets -> Perm
dotAgainstForPerm bras kets = error "TODO"

-- NOTE: Unchecked preconditions:
--  * All bras/kets are normalized to a self-inner-product of 1.
--  * The 's' values uniquely label each bra. (none appear twice)
popClosestBra :: (Eq s)=> [(s, Ket)] -> Ket -> ((s, Ket), [(s, Ket)])
popClosestBra []      _   = error "popClosestBra: empty list"
popClosestBra allBras ket = rec 1 0 (error "popClosestBra: buggy bug!") allBras
  where
    rec remainingProb bestProb bestBra (thisBra:bras)
        | remainingProb < bestProb = (bestBra, otherBras bestBra)
        | thisProb > bestProb = rec remainingProb' thisProb thisBra bras
        | otherwise           = rec remainingProb' bestProb bestBra bras
      where
        thisProb = sqMagnitude (snd thisBra `ketDot` ket)
        remainingProb' = remainingProb - thisProb
        otherBras bra = findDelete ((fst bra ==) . fst) allBras

-- Good thing the stdlib provided us with the extremely fundamental 'deleteBy', which
-- generalizes over, uhm... wait, equality comparators!? That can't be right...
-- hm.....yep, that's all they gave us.
findDelete :: (a -> Bool) -> [a] -> [a]
findDelete f []     = []
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
                    , matchTolNonDegenerateMinDiff = 1e-7
                    , matchTolMaxGuessError        = 1e-6
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
        in case find (\x -> matchTolMaxGuessError < x) diffs of
            Just x -> Left $ "Guess is too far from actual! (difference of " ++ show x ++ ")"
            Nothing -> Right ()

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
extrapolate :: (Real x, Floating y, _)=> [x] -> [y] -> x -> y
extrapolate px py x = result
  where
    result = head . expect "extrapolate: degenerate matrix (duplicate x position?)"
                  $ solveLinearSystem mat py
    degree = length px - 1
    mat = [[realToFrac (p - x) ** realToFrac k | k <- [0..degree]] | p <- px ]

solveLinearSystem :: (Floating a, _)=> [[a]] -> [a] -> Maybe [a]
solveLinearSystem a b = Matrix.toList . Matrix.flatten
                        <$> Matrix.linearSolve (Matrix.fromRows . fmap Matrix.fromList $ a)
                                               (Matrix.asColumn . Matrix.fromList $ b)


expect :: String -> Maybe a -> a
expect msg = maybe (error msg) id

{-
main' :: ByteString -> ByteString
main' s = output where
	Just input = Aeson.decode s :: Maybe [[[Double]]]
	(dat,perms) = reorder input
	output = Aeson.encode $ Aeson.object
		[ "data" .= dat
		, "perms" .= perms
		, "gamma-ids" .= (argSort <$> perms)
		]

main :: IO ()
main = do --ByteString.interact main'
    -- bands [[1,6,6],[3,6,6],[5,6,6],[6,6,7],[6,6,9]]
    print $ (List.sort $ concat $ concat $ [[[a+b+c | a <- [1,2,3,4,5]]
                     | b <- [10,20,30,40,50]]
                     | c <- [100,200,300,400,500]])
         ==  (List.sort $ concat $ concat $ fst . reorder $
            [[[a+b+c | a <- [1,2,3,4,5]]
                     | b <- [10,20,30,40,50]]
                     | c <- [100,200,300,400,500]])
--main = mapM_ print $ bands [[1,6],[3,6],[6,5],[6,7],[6,9]]

indices :: [a] -> Vector Int
indices xs = Vector.fromList [0 .. length xs - 1]

argSort :: (Foldable t, Ord a) => t a -> Perm
argSort xs = Vector.fromList $ List.sortOn (vs !) [0 .. length xs - 1] where
	vs = Vector.fromList (toList xs)

permute :: [a] -> Perm -> [a]
permute xs perm = Vector.toList $ Vector.backpermute (Vector.fromList xs) perm

scanl2 :: (b -> b -> a -> b) -> b -> b -> [a] -> [b]
scanl2 f b0 b1 [] = [b0,b1]
scanl2 f b0 b1 (a:as) = b0:scanl2 f b1 (f b0 b1 a) as

fixBands :: (Num a, Ord a) => Perm -> [[a]] -> [(Perm, [a])]
fixBands initPerm xs = run (List.sort <$> xs) where
	run xs = scanl2 f (initItem (xs!!0)) (initItem (xs!!1)) (drop 2 xs)
	f prev cur orig = (perm, next) where
		perm = (argSort.argSort) guess
		guess = zipWith (-) ((2*) <$> snd cur) (snd prev)
		next = permute (List.sort orig) perm
	initItem x = (initPerm, permute (List.sort x) initPerm)

reorderChunk :: (Num a, Ord a) => Perm -> [[a]] -> ([[a]], Perm)
reorderChunk initPerm xs = (theBands, finalPerm) where
	fixed = fixBands initPerm xs
	theBands = fmap snd $ fixed
	finalPerm = fst . last $ fixed

reorder :: (Num a, Ord a) => [[[a]]] -> ([[[a]]],[Perm])
reorder chunks = List.unzip $ rec (indices $ head $ head $ chunks) chunks where
	rec initPerm [] = []
	rec initPerm (chunk:chunks) =
		let (fixed, finalPerm) = reorderChunk initPerm chunk
		in  (fixed, initPerm):rec finalPerm chunks

traceShowOn :: (Show b) => (a -> b) -> a -> a
traceShowOn f x = traceShow (f x) x

shape :: [[[a]]] -> (Int,Int,Int)
shape xs = (length xs, length $ head xs, length . head . head $ xs)

makeExample :: IO [[[Double]]]
makeExample =
	fmap (fmap List.sort <$>) .
	fmap (List.transpose <$>) .
	fmap (List.transpose) .
	(sequence . replicate 100) $ makeChunkedData

makeChunkedData :: IO [[Double]]
makeChunkedData = do
	first  <- makeCurveData
	second <- makeCurveData
	let second' = (+ last first) . (subtract $ head second) <$> second
	return [first, second']

randomPoly :: IO (Double -> Double)
randomPoly = do
	a <- randomIO :: IO Double
	b <- randomIO :: IO Double
	c <- randomIO :: IO Double
	x0 <- randomIO :: IO Double
	return $ \x -> let d = (x-x0) in (1-2*a)*d*d + (1-2*b)*d + c

makeCurveData :: IO ([Double])
makeCurveData = do
	poly <- randomPoly
	return $ poly <$> [0, 1/100.. 1]
-}

main = pure ()
