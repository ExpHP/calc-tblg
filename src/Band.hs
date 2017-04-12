{-# LANGUAGE TupleSections #-}
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
import           "base" Data.Function((&))
import           "base" Data.Ord (comparing)
import           "base" System.Exit(exitWith, ExitCode(..))
import qualified "base" Data.List as List
import           "containers" Data.Map(Map)
import qualified "containers" Data.Map as Map
import           "aeson" Data.Aeson((.=))
import qualified "aeson" Data.Aeson as Aeson
import qualified "aeson" Data.Aeson.Types as Aeson
import qualified "yaml" Data.Yaml as Yaml
import           "bytestring" Data.ByteString.Lazy(ByteString)
import qualified "bytestring" Data.ByteString.Lazy as ByteString
import qualified "bytestring" Data.ByteString as ByteString.ByWhichIMeanTheOtherByteString
import           "random" System.Random
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

systemDirName :: System -> FilePath
systemDirName SystemA = "a"
systemDirName SystemB = "b"

type Ket = Vector (Complex Double)
type Kets = Vector Ket
type Perm = Vector Int
type Energies = Vector Double

data Strategy = Identity
              | DotAgainst !System !(LineId, Int)
              | Extrapolate [Int]

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
    f s (h, i) | center < i  = Extrapolate [i-1, i-2, i-3]
    f s (h, i) | i < center  = Extrapolate [i+1, i+2, i+3]

linspace :: (Fractional a)=> Int -> a -> a -> [a]
linspace n a b = [ (a * realToFrac (n-1-k) + b * realToFrac k) / realToFrac (n-1)
                 | k <- [0..n-1] ]

computePermAccordingToStrategy :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> Strategy -> io Perm
computePermAccordingToStrategy u _ _ Identity = pure $ Vector.fromList [0..uncrosserNBands u - 1]

computePermAccordingToStrategy u ketS ketQ (DotAgainst braS braQ) = do
    [kets] <- needOriginalVectors u ketS [ketQ]
    [bras] <- needUncrossedVectors u braS [braQ]
    pure $ dotAgainstForPerm bras kets

computePermAccordingToStrategy u sys thisQ@(line, thisI) (Extrapolate otherIs) = do
    otherEsByBand <- Vector.fromList . List.transpose . fmap toList
                     <$> needUncrossedEnergies u sys ((line,) <$> otherIs)
    [theseEs] <- needOriginalEnergies u sys [thisQ]
    let guessEs = fmap (\otherEs -> extrapolate otherIs otherEs thisI) otherEsByBand

    case matchEnergiesForPerm guessEs theseEs of
        Left err -> do
            warn $ "Troublesome energies at " ++ show (sys, thisQ) ++ ": (" ++ err ++ ")"
            computePermAccordingToStrategy u sys thisQ (DotAgainst sys (line, nearest thisI otherIs))
        Right x -> pure x

warn :: (MonadIO io)=> String -> io ()
warn = liftIO . hPutStrLn stderr

nearest :: (Num a, Ord a, Foldable t)=> a -> t a -> a
nearest x = minimumBy (comparing (abs . subtract x)) . toList

-----------------------------------------------------------------
-- requesting kpoints

type KPoint = [Double]

needOriginalEnergies :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Energies]
needOriginalEnergies u s = liftIO . askEigenvalues (uncrosserOracle u s)

needOriginalVectors :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Kets]
needOriginalVectors = eigenvectorCacheRequest

needAllPermutations :: (MonadIO io)=> Uncrosser -> System -> io [Perm]
needAllPermutations u s = needPermutations u s (oracleKIds $ uncrosserOracle u s)

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
            writeIORef ref (Done val)

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
eigenvectorCacheRequest :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io [Kets]
eigenvectorCacheRequest u s q = eigenvectorCacheEnsure u s q
                                >> mapM (eigenvectorCacheGetSingle u s) q

-- make sure things are in the cache
eigenvectorCacheEnsure :: (MonadIO io)=> Uncrosser -> System -> [(LineId, Int)] -> io ()
eigenvectorCacheEnsure u s q = do
    idx <- filterM (eigenvectorCacheHasSingle u s) q
    vecs <- computeEigenvectors u s q
    zipWithM_ (eigenvectorCachePutSingle u s) q vecs

eigenvectorCacheHasSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Bool
eigenvectorCacheHasSingle u s q = eigenvectorCachePath u s q >>= liftIO . doesFileExist

-- get something that is known to be in the cache
eigenvectorCacheGetSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io Kets
eigenvectorCacheGetSingle u s q = eigenvectorCachePath u s q >>= liftIO . fmap ketsFromCereal . readJson

eigenvectorCachePutSingle :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> Kets -> io ()
eigenvectorCachePutSingle u s q kets = eigenvectorCachePath u s q
                                       >>= liftIO . flip writeJson (ketsToCereal kets)

eigenvectorCachePath :: (MonadIO io)=> Uncrosser -> System -> (LineId, Int) -> io FilePath
eigenvectorCachePath u s (LineId h, i) = do
    let dir = uncrosserWorkDir u </> "eigenvecs"
    liftIO $ createDirectoryIfMissing False dir  -- XXX this function ought to be pure...
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

dotAgainstForPerm :: Kets -> Kets -> Perm
dotAgainstForPerm a b = Vector.fromList $ rec (toList a) (zip [0..] $ toList b)
  where
    rec [] [] = []
    rec (bra:bras') kets = let ((i,_), kets') = popClosestKet bra kets
                           in i : rec bras' kets'
    rec _ _ = error "dotAgainstForPerm: unequal number of bras and kets!"

popClosestKet :: (Eq s)=> Ket -> [(s, Ket)] -> ((s, Ket), [(s, Ket)])
popClosestKet = flip popClosestBra


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

    BandJsonParseData{..} <- readJson "eigenvalue.json"
    HighSymInfo{..} <- readJson "hsym.json"
    expectFile "oracle.conf"

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
        bandStrParts = qs >>= \(LineId h,i) -> oraclePoints_ o ! h ! i -- o hi there
    let bandStr = List.intercalate " " (show <$> bandStrParts)

    callProcess "phonopy" ["oracle.conf", "--band_points=1", "--band=" ++ bandStr]

    vecs <- fromEigenvectorParseData <$> readYaml "band.yaml"
    removeFile "band.yaml"
    pure . toList $ vecs

askToWriteCorrectedFile :: Oracle -> Vector Perm -> IO ()
askToWriteCorrectedFile o perms =
    (permuteBandYaml perms <$> readJson "eigenvalue.json") >>= writeJson "eigenvalue.json"

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

        postprocess yaml@BandYaml{..} = BandJsonParseData{..} where
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

kpointLinspace n kFrom kTo = result
  where
    ZipList segmentPointsZip = linspace n <$> ZipList kFrom <*> ZipList kTo
    result = Vector.fromList $ List.transpose segmentPointsZip




main = pure ()


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

onlyUniqueValue :: (Eq a, Foldable t)=> t a -> Maybe a
onlyUniqueValue = onlyValue . List.nub . toList -- This usage of nub is O(n) in the common case.

expect :: String -> Maybe a -> a
expect msg = maybe (error msg) id
