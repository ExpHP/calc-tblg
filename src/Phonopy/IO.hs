
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}

module Phonopy.IO where

import           "exphp-prelude" ExpHPrelude hiding (transpose)
import qualified "base" Data.List as List
import           "base" System.Exit(ExitCode(..))
import qualified "aeson" Data.Aeson as Aeson
import           "directory" System.Directory
import           "process" System.Process
import qualified "vector" Data.Vector as Vector
import           Control.Lens(over,view,set)

import           FunctorUtil(ffmap)
import           JsonUtil
import           Phonopy.BandYaml(BandYaml(..))
import qualified Phonopy.BandYaml as BandYaml
import qualified Phonopy.BandYaml.Npy as BandYaml.Npy
import           Phonopy.Types
import           Band.Aliases

-- Read energies from a band.yaml.
readQPathEnergies :: FilePath -> IO (QPathData Energies)
readQPathEnergies fp = do
    BandJsonParseData{..} <- readYaml fp
    pure . QPathData . Vector.fromList $ partitionVector (toList bandJsonLineLengths)
                                                         bandJsonEnergies
  where
    -- split into chunks of precisely the given sizes, which must add up to the total size
    partitionVector []     v | null v       = []
                             | otherwise    = error "partitionVector: Vector longer than total output length"
    partitionVector (n:ns) v | n > length v = error "partitionVector: Vector shorter than total output length"
                             | otherwise    = Vector.take n v : partitionVector ns (Vector.drop n v)

-- | Run phonopy in a given directory to compute eigenvectors at a given point.
--   Input files must already be set up; existing output files will be clobbered.
unsafeComputeEigenvectors :: [String] -> FilePath -> [QVec] -> IO [Kets]
unsafeComputeEigenvectors args root qs =
  withCurrentDirectory root $ do
    let bandStr = List.intercalate " " (show <$> ((qs >>= toList) ++ [0,0,0]))

    child <- runProcess
     {- cmd -} "phonopy"
     {- arg -} (args ++ ["--eigenvectors", "--band_points=1", "--band=" ++ bandStr])
     {- cwd -} Nothing
     {- env -} (Just [("EIGENVECTOR_NPY_HACK", "1")])
     {-  in -} Nothing
     {- out -} Nothing
     {- err -} Nothing

    waitForProcess child >>= \case
      ExitSuccess   -> pure ()
      ExitFailure r -> fail $ "computeEigenvectors: phonopy exited with code " ++ show r

    vecs <- BandYaml.Npy.readKetsFile "eigenvector.npy"
    removePathForcibly "eigenvector.npy"
    removePathForcibly "band.yaml"
    pure . toList $ vecs

---------------------------------------

data BandJsonParseData = BandJsonParseData
  { bandJsonNAtoms :: Int
  , bandJsonLineLengths :: Vector Int
  , bandJsonEnergies :: Vector Energies
  }

instance Aeson.FromJSON BandJsonParseData where
    parseJSON = Aeson.parseJSON >>> fmap postprocess where

        postprocess BandYaml{..} = BandJsonParseData{..} where
            bandJsonLineLengths = Vector.fromList _bandYamlSegmentNQPoint
            bandJsonNAtoms = _bandYamlNAtom
            bandJsonEnergies = _bandYamlSpectrum                 -- :: Vector SpectrumData
                               & fmap BandYaml._spectrumBand     -- :: Vector (Vector DataBand)
                               & ffmap BandYaml._bandFrequency   -- :: Vector (Vector Energies)

permuteBandYaml :: Vector Perm -> BandYaml -> BandYaml
permuteBandYaml perms = over BandYaml.bandYamlSpectrum (Vector.zipWith permuteEntry perms)
  where permuteEntry perm = over BandYaml.spectrumBand (`Vector.backpermute` perm)

putBandYamlSpectrum :: Vector Energies -> BandYaml -> BandYaml
putBandYamlSpectrum allEs = over BandYaml.bandYamlSpectrum (Vector.zipWith putEntry allEs)
  where putEntry es = set BandYaml.spectrumBand (fmap (\e -> BandYaml.DataBand e Nothing) es)

getBandYamlSpectrum :: BandYaml -> Vector Energies
getBandYamlSpectrum = ffmap (view BandYaml.bandFrequency)
                    . fmap (view BandYaml.spectrumBand)
                    . view BandYaml.bandYamlSpectrum

-----------------------------------------------------------------
