
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}

module Phonopy.IO where

import           "exphp-prelude" ExpHPrelude hiding (transpose)
import           "base" System.Exit(exitWith, ExitCode(..))
import qualified "base" Data.List as List
import qualified "containers" Data.Map as Map
import qualified "aeson" Data.Aeson as Aeson
import           "directory" System.Directory
import           "process" System.Process
import           "vector" Data.Vector((!))
import qualified "vector" Data.Vector as Vector
import           "linear" Linear.V3

import           GeneralUtil(ffmap)
import           JsonUtil
import           Phonopy.BandYaml(BandYaml(..))
import qualified Phonopy.BandYaml as BandYaml
import qualified Phonopy.BandYaml.Npy as BandYaml.Npy
import           Phonopy.Types
import           Band.Aliases

data OracleLineData = OracleLineData
  { segmentPoints :: Vector (V3 Double)  -- ^ The reciprocal-space coords of each point.
  , segmentOriginalEs :: Vector Energies -- ^ Band energies at each point, in original order.
  }

readQPathEnergies :: FilePath -> IO (QPathData Energies)
readQPathEnergies fp = do
    BandJsonParseData{..} <- readYaml fp
    pure . QPathData . Vector.fromList $ partitionVector (toList bandJsonLineLengths)
                                                         bandJsonEnergies

-- XXX
askEigenvectors :: FilePath -> QPath -> [(LineId, Int)] -> IO [Kets]
askEigenvectors root qpath = askEigenvectorsVia (\_ k -> pure k) root qpath

-- XXX
-- Request eigenvectors, letting them be streamed through a callback.
askEigenvectorsVia :: ((LineId, Int) -> Kets -> IO a) -> FilePath -> QPath -> [(LineId, Int)] -> IO [a]
askEigenvectorsVia cb root qpath qs =
    fmap concat . mapM (\chk -> askEigenvectors' root qpath chk >>= zipWithM cb chk)
        $ chunk 150 qs -- chunk to use less memory

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- XXX
askEigenvectors' :: FilePath -> QPath -> [(LineId, Int)] -> IO [Kets]
askEigenvectors' root o qids =
    unsafeComputeEigenvectors root
    $ fmap (\(LineId h,i) -> qPathDataByLine o ! h ! i) qids

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

---------------------------------------
-- ugly parts of Oracle that remain

-- XXX
askToWriteCorrectedFile :: FilePath -> Vector Perm -> IO ()
askToWriteCorrectedFile root perms =
  withCurrentDirectory root $
    permuteBandYamlFile "eigenvalues.yaml" "corrected.yaml" perms

-- XXX
askToWriteNamedFile :: FilePath -> FilePath -> Vector Energies -> IO ()
askToWriteNamedFile root fp energies =
  withCurrentDirectory root $
    putBandYamlFileSpectrum "eigenvalues.yaml" fp energies

---------------------------------------

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

-----------------------------------------------------------------
