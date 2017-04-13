
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------
-- The oracle. It knows about the problem we are trying to solve.
-- (where does the eigensystem come from? What do we want to produce in the end?)

module Band.Oracle.Phonopy where

import           "base" Data.Foldable
import           "base" Data.Complex
import           "base" Control.Applicative
import           "base" Control.Monad
import           "base" Control.Arrow
import           "base" Data.Function((&))
import           "base" System.Exit(exitWith, ExitCode(..))
import qualified "base" Data.List as List
import           "containers" Data.Map(Map)
import qualified "containers" Data.Map as Map
import qualified "aeson" Data.Aeson as Aeson
import           "directory" System.Directory
import           "process" System.Process
import           "vector" Data.Vector(Vector,(!))
import qualified "vector" Data.Vector as Vector

import           GeneralUtil(ffmap,fffmap,ffffmap,bool)
import           JsonUtil
import           Band.Oracle.API
import           Band.Oracle.Phonopy.BandYaml(BandYaml(..))
import qualified Band.Oracle.Phonopy.BandYaml as BandYaml

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
askEigenvectors o qs =
  withCurrentDirectory (rootDir o) $ do
    let bandStrParts :: [Double]
        bandStrParts = (qs >>= \(LineId h,i) -> points_ o ! h ! i) -- o hi there
                       ++ [0,0,0] -- one extra point for the "end" of the last segment
    let bandStr = List.intercalate " " (show <$> bandStrParts)

    -- FIXME --readfc should be punted to the user's oracle.conf, like --hdf5 is.
    callProcess "phonopy" ["oracle.conf", "--readfc", "--eigenvectors", "--band_points=1", "--band=" ++ bandStr]

    vecs <- fromEigenvectorParseData <$> readYaml "band.yaml"
    removeFile "band.yaml"
    pure . toList $ vecs

askToWriteCorrectedFile :: Oracle -> Vector Perm -> IO ()
askToWriteCorrectedFile o perms =
  withCurrentDirectory (rootDir o) $ do
    (permuteBandYaml perms <$> readYaml "eigenvalues.yaml") >>= writeYaml "corrected.yaml"

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

type HSymPoint = String

kpointLinspace :: Fractional a => Int -> [a] -> [a] -> Vector [a]
kpointLinspace n kFrom kTo = result
  where
    ZipList segmentPointsZip = linspace n <$> ZipList kFrom <*> ZipList kTo
    result = Vector.fromList $ List.transpose segmentPointsZip

linspace :: (Fractional a)=> Int -> a -> a -> [a]
linspace n a b = [ (a * realToFrac (n-1-k) + b * realToFrac k) / realToFrac (n-1)
                 | k <- [0..n-1] ]
