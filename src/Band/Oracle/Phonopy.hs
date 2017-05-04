{-# LANGUAGE ParallelListComp #-}


{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}

-- FIXME Kill this file.
--       All I really want are just some straightforward-to-use IO functions.
--       This is a textbook example of premature abstractions, and it's no
--       wonder that half of the frustration I'm having with adding new code
--       is related to this module.

{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------
-- The oracle. It knows about the problem we are trying to solve.
-- (where does the eigensystem come from? What do we want to produce in the end?)

module Band.Oracle.Phonopy where

import           "exphp-prelude" ExpHPrelude hiding (transpose)
import           "base" Control.Applicative
import           "base" System.Exit(exitWith, ExitCode(..))
import qualified "base" Data.List as List
import qualified "containers" Data.Map as Map
import qualified "aeson" Data.Aeson as Aeson
import           "directory" System.Directory
import           "process" System.Process
import           "vector" Data.Vector((!))
import qualified "vector" Data.Vector as Vector
import qualified "vector" Data.Vector.Unboxed as UVector
import           "linear" Linear.V3
import           "linear" Linear.Matrix


import           GeneralUtil(ffmap)
import           JsonUtil
import           Band.Oracle.API
import           Band.Oracle.Phonopy.BandYaml(BandYaml(..))
import qualified Band.Oracle.Phonopy.BandYaml as BandYaml
-- non-backtracking parser for eigenkets (for O(1) memory overhead)
import qualified Band.Oracle.Phonopy.BandYaml.Npy as BandYaml.Npy

type KPoint = V3 Double

data Oracle = Oracle
  { rootDir :: FilePath
  , nBands :: Int
  , hSymPath :: HSymPath
  , energies_ :: Vector (Vector Energies)
  }

data OracleLineData = OracleLineData
  { segmentPoints :: Vector KPoint       -- ^ The reciprocal-space coords of each point.
  , segmentOriginalEs :: Vector Energies -- ^ Band energies at each point, in original order.
  }

initOracle :: FilePath -> IO Oracle
initOracle rootDir =
  withCurrentDirectory rootDir $ do

    let expectFile s = doesPathExist s >>= bool (fail $ "expected file: " ++ s) (pure ())

    BandJsonParseData{..} <- readYaml "eigenvalues.yaml"
    HighSymInfo{..} <- readJson "hsym.json"
    expectFile "oracle.conf"
    expectFile "POSCAR" -- phonopy wants this for "symmetry"...
    expectFile "FORCE_SETS"

    let nBands = 3 * bandJsonNAtoms
    let lineLengths_ = bandJsonLineLengths
    let hSymMap = highSymInfoPoints
    let hSymPoints = highSymInfoPath

    let hSymPath = mkHSymPath ((hSymMap Map.!) <$> toList hSymPoints) (toList lineLengths_)

    let energies_ = Vector.fromList $ partitionVector (toList bandJsonLineLengths)
                                                      bandJsonEnergies

    pure $ Oracle
        { rootDir
        , nBands
        , energies_
        , hSymPath
        }

-- FIXME HACK it really should not have to be this complicated to read a kpath!
abuseOracleFrameworkToGetKPath :: FilePath -> IO HSymPath
abuseOracleFrameworkToGetKPath rootDir = 
  withCurrentDirectory rootDir $ do

    BandJsonParseData{..} <- readYaml "eigenvalues.yaml"
    HighSymInfo{..} <- readJson "hsym.json"

    let lineLengths_ = bandJsonLineLengths
    let hSymMap = highSymInfoPoints
    let hSymPoints = highSymInfoPath

    pure $ mkHSymPath ((hSymMap Map.!) <$> toList hSymPoints) (toList lineLengths_)


askEigenvalues :: Oracle -> [(LineId, Int)] -> IO [Energies]
askEigenvalues o qs = pure $
    flip map qs $ \(LineId h, i) ->
        energies_ o ! h ! i

askEigenvectors :: Oracle -> [(LineId, Int)] -> IO [Kets]
askEigenvectors = askEigenvectorsVia (\_ k -> pure k)

interceptIO :: (a -> IO b) -> (a -> IO a)
interceptIO f x = f x >> pure x

-- Request eigenvectors, letting them be streamed through a callback.
askEigenvectorsVia :: ((LineId, Int) -> Kets -> IO a) -> Oracle -> [(LineId, Int)] -> IO [a]
askEigenvectorsVia cb o qs =
    fmap concat . mapM (\chk -> askEigenvectors' o chk >>= zipWithM cb chk)
        $ chunk 150 qs -- chunk to use less memory

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

askEigenvectors' :: Oracle -> [(LineId, Int)] -> IO [Kets]
askEigenvectors' o qids =
    unsafeComputeEigenvectors (rootDir o)
    $ fmap (\(LineId h,i) -> pathQPointsByLine (hSymPath o) ! h ! i) qids

unsafeComputeEigenvectors :: FilePath -> [V3 Double] -> IO [Kets]
unsafeComputeEigenvectors root qs =
  withCurrentDirectory root $ do
    let bandStr = List.intercalate " " (show <$> ((qs >>= toList) ++ [0,0,0]))

    -- FIXME --readfc should be punted to the user's oracle.conf, like --hdf5 is.
    callProcess "phonopy" ["oracle.conf", "--readfc", "--eigenvectors", "--band_points=1", "--band=" ++ bandStr]

    vecs <- BandYaml.Npy.readKetsFile "eigenvector.npy"
    removeFile "eigenvector.npy"
    removeFile "band.yaml"
    pure . toList $ vecs

-- XXX
askToWriteCorrectedFile :: Oracle -> Vector Perm -> IO ()
askToWriteCorrectedFile o perms =
  withCurrentDirectory (rootDir o) $
    permuteBandYamlFile "eigenvalues.yaml" "corrected.yaml" perms 

-- XXX
askToWriteNamedFile :: Oracle -> FilePath -> Vector Energies -> IO ()
askToWriteNamedFile o fp energies =
  withCurrentDirectory (rootDir o) $
    putBandYamlFileSpectrum "eigenvalues.yaml" fp energies
    
permuteBandYamlFile :: FilePath -> FilePath -> Vector Perm -> IO ()
permuteBandYamlFile inPath outPath perms =
    (permuteBandYaml perms <$> readYaml inPath) >>= writeYaml outPath

putBandYamlFileSpectrum :: FilePath -> FilePath -> Vector Energies -> IO ()
putBandYamlFileSpectrum inPath outPath energies =
    (putBandYamlSpectrum energies <$> readYaml inPath) >>= writeYaml outPath
    
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
        HighSymInfo <$> (fmap (\[a,b,c] -> V3 a b c) <$> o Aeson..: "point")
                    <*> o Aeson..: "path"

instance Aeson.FromJSON BandJsonParseData where
    parseJSON = Aeson.parseJSON >>> fmap postprocess where

        postprocess BandYaml{..} = BandJsonParseData{..} where
            bandJsonLineLengths = Vector.fromList bandYamlSegmentNQPoint
            bandJsonNAtoms = bandYamlNAtom
            bandJsonEnergies = bandYamlSpectrum                 -- :: Vector SpectrumData
                               & fmap BandYaml.spectrumBand     -- :: Vector (Vector DataBand)
                               & ffmap BandYaml.bandFrequency   -- :: Vector (Vector Energies)

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

putBandYamlSpectrum :: Vector Energies -> BandYaml -> BandYaml
putBandYamlSpectrum es yaml = yaml'
  where
    spectrum = bandYamlSpectrum yaml
    spectrum' = Vector.zipWith putBandYamlSpectrumEntry es spectrum
    yaml' = debug $ yaml{bandYamlSpectrum = spectrum'}
    -- debug = traceWith (Vector.take 5.bandYamlSpectrum)
    debug = id

putBandYamlSpectrumEntry :: Energies -> BandYaml.SpectrumData -> BandYaml.SpectrumData
putBandYamlSpectrumEntry es dat = dat'
  where
    bands' = fmap (\e -> BandYaml.DataBand e Nothing) es
    dat' = dat{BandYaml.spectrumBand = bands'}

getBandYamlSpectrum :: BandYaml -> Vector Energies
getBandYamlSpectrum = fmap (fmap BandYaml.bandFrequency)
                    . fmap BandYaml.spectrumBand
                    . bandYamlSpectrum

exitOnFailure :: ExitCode -> IO ()
exitOnFailure ExitSuccess = pure ()
exitOnFailure e = exitWith e

type HSymPoint = String

-----------------------------------------------------------------

-- relationship of highsym path to individual qpoints
data HSymPath = HSymPath
    { pathQPointsByLine :: Vector (Vector (V3 Double))
    }

pathLineIds :: HSymPath -> [LineId]
pathLineIds p = LineId <$> [0..length (pathQPointsByLine p) - 1]
pathLineLength :: HSymPath -> LineId -> Int
pathLineLength p (LineId h) = length (pathQPointsByLine p ! h)
pathAllIds :: HSymPath -> [(LineId, Int)]
pathAllIds p = pathLineIds p >>= \h -> (h,) <$> [0..pathLineLength p h - 1]

mkHSymPath :: [V3 Double] -> [Int] -> HSymPath
mkHSymPath points lengths
    | length points /= length lengths + 1 = error "mkHSymPath: incompatible band/points lengths"
    | otherwise = result
  where
    pointsV = Vector.fromList points
    lengthsV = Vector.fromList lengths
    offsetsV = Vector.prescanl' (+) 0 lengthsV
    qpointsByLineV :: Vector (Vector (V3 Double))
    qpointsByLineV =
        fmap (\(V3 as bs cs) -> Vector.zipWith3 V3 as bs cs)
        [ phonopyLinspaceV n <$> q1 <*> q2 | n <- lengthsV
                                           | q1 <- Vector.init pointsV
                                           | q2 <- Vector.tail pointsV
                                           ]
    phonopyLinspaceV n a b = Vector.fromList $ phonopyLinspace n a b

    result = HSymPath qpointsByLineV

-- NOTE: consistent with how phonopy does Q Paths, this function:
--  * Will just yield [a] if n == 1.
--  * Will include both endpoints otherwise.
phonopyLinspace :: (Fractional a)=> Int -> a -> a -> [a]
phonopyLinspace n a b = phonopyLinspaceItem n a b <$> [0..n-1]

-- Single item of a linspace
phonopyLinspaceItem :: (Fractional a)=> Int -> a -> a -> Int -> a
phonopyLinspaceItem 1 a _ 0 = a
phonopyLinspaceItem n a b k = let n' = realToFrac n
                                  k' = realToFrac k
                              in (a * (n'-1-k') + b * k') / (n'-1)

-----------------------------------------------------------------
