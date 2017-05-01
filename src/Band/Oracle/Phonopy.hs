
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

{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------
-- The oracle. It knows about the problem we are trying to solve.
-- (where does the eigensystem come from? What do we want to produce in the end?)

module Band.Oracle.Phonopy where

import           "exphp-prelude" ExpHPrelude
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


import           GeneralUtil(ffmap)
import           JsonUtil
import           Band.Oracle.API
import           Band.Oracle.Phonopy.BandYaml(BandYaml(..))
import qualified Band.Oracle.Phonopy.BandYaml as BandYaml
-- non-backtracking parser for eigenkets (for O(1) memory overhead)
import qualified Band.Oracle.Phonopy.BandYaml.Npy as BandYaml.Npy

type KPoint = [Double]

data Oracle = Oracle
  { rootDir :: FilePath
  , nBands :: Int
  , hSymPoints :: Map HSymPoint KPoint
  , hSymPath :: Vector HSymPoint
  , lineLengths_ :: Vector Int
  , points_ :: Vector (Vector KPoint)
  , energies_ :: Vector (Vector Energies)
  }

data OracleLineData = OracleLineData
  { segmentPoints :: Vector KPoint       -- ^ The reciprocal-space coords of each point.
  , segmentOriginalEs :: Vector Energies -- ^ Band energies at each point, in original order.
  }


lineIds :: Oracle -> [LineId]
lineIds o = LineId <$> [0..length (hSymPath o) - 2]
lineLength :: Oracle -> LineId -> Int
lineLength o (LineId h) = lineLengths_ o Vector.! h
kIds :: Oracle -> [(LineId, Int)]
kIds o = lineIds o >>= \h -> (h,) <$> [0..lineLength o h - 1]

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
    let hSymPoints = highSymInfoPoints
    let hSymPath = highSymInfoPath

    let pathKPoints = (hSymPoints Map.!) <$> hSymPath
    let points_ = Vector.zipWith3 kpointLinspace lineLengths_
                                                 (Vector.init pathKPoints)
                                                 (Vector.tail pathKPoints)

    let energies_ = Vector.fromList $ partitionVector (toList bandJsonLineLengths)
                                                      bandJsonEnergies

    pure $ Oracle
        { rootDir
        , nBands
        , lineLengths_
        , energies_
        , hSymPoints
        , hSymPath
        , points_
        }

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
askEigenvectors' o qs =
  withCurrentDirectory (rootDir o) $ do
    let bandStrParts :: [Double]
        bandStrParts = (qs >>= \(LineId h,i) -> points_ o ! h ! i) -- o hi there
                       ++ [0,0,0] -- one extra point for the "end" of the last segment
    let bandStr = List.intercalate " " (show <$> bandStrParts)

    -- listDirectory "." >>= (filter ("edit.yaml-" `isPrefixOf`) >>> (mapM_ removeFile))

    -- FIXME --readfc should be punted to the user's oracle.conf, like --hdf5 is.
    callProcess "phonopy" ["oracle.conf", "--readfc", "--eigenvectors", "--band_points=1", "--band=" ++ bandStr]

    -- -- Attention, this is your captain speaking;
    -- -- We're about to make an egregious breaking of abstraction here.
    -- -- Buckle your seat belts, and sorry for the inconvenience.
    -- callProcess "../../../../../scripts/preprocess-band-yaml" []

    -- fnames <- (sort . filter ("edit.yaml-" `isPrefixOf`)) <$> listDirectory "."

    -- vecs <- forM fnames $ \fname ->
    --         BandYaml.Preprocessed.readKetsFile fname <* removeFile fname
    vecs <- BandYaml.Npy.readKetsFile "eigenvector.npy"
    traceIO ("LENGTH: " ++ show (length vecs))
    traceIO ("MENGTH: " ++ show (length (vecs ! 0)))
    traceIO ("NENGTH: " ++ show (UVector.length (vecs ! 0 ! 0)))
    -- removeFile "eigenvector.npy"

    -- listDirectory "." >>= (filter ("edit.yaml-" `isPrefixOf`)
    --                        >>> (mapM_ (error . ("unused file: " ++))))

--    vecs <- BandYaml.LL1.readBandYamlKets "band.yaml"
    removeFile "band.yaml"
    pure . toList $ vecs
    -- pure vecs :: IO [Kets]

askToWriteCorrectedFile :: Oracle -> Vector Perm -> IO ()
askToWriteCorrectedFile o perms =
  withCurrentDirectory (rootDir o) $ do
    (permuteBandYaml perms <$> readYaml "eigenvalues.yaml") >>= writeYaml "corrected.yaml"

askToWriteNamedFile :: Oracle -> FilePath -> Vector Energies -> IO ()
askToWriteNamedFile o fp energies =
  withCurrentDirectory (rootDir o) $ do
    (putBandYamlSpectrum energies <$> readYaml "eigenvalues.yaml") >>= writeYaml fp

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

exitOnFailure :: ExitCode -> IO ()
exitOnFailure ExitSuccess = pure ()
exitOnFailure e = exitWith e

type HSymPoint = String

kpointLinspace :: Fractional a => Int -> [a] -> [a] -> Vector [a]
kpointLinspace n kFrom kTo = result
  where
    ZipList segmentPointsZip = linspace n <$> ZipList kFrom <*> ZipList kTo
    result = Vector.fromList $ List.transpose segmentPointsZip

linspace :: (Fractional a)=> Int -> a -> a -> [a]
linspace n a b = [ (a * realToFrac (n-1-k) + b * realToFrac k) / realToFrac (n-1)
                 | k <- [0..n-1] ]
