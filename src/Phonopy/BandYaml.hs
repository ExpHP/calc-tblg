{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- (NOTE: do not use the types from here to parse eigenvectors; fromJSON is a non-streaming
--        interface which deserializes the entire file to a Value, and the resulting memory
--        overhead for eigenvectors gets really bad, really fast.  See Phonopy.Npy)

-- Raw types for parsing band.yaml,
-- with straightforward ToJSON/FromJSON implementations.

module Phonopy.BandYaml where

import           "exphp-prelude" ExpHPrelude
import           "aeson" Data.Aeson
import           "aeson" Data.Aeson.Types
import           "aeson" Data.Aeson.TH
import           "lens" Control.Lens(makeLenses)

---------------------------------------------------------

data DataBand = DataBand
    { _bandFrequency :: Double
    , _bandEigenvector :: Maybe (Vector (Vector (Double, Double)))
    } deriving (Eq, Show, Read)

$(let f "_bandFrequency" = "frequency"
      f "_bandEigenvector" = "eigenvector"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''DataBand)
makeLenses ''DataBand

---------------------------------------------------------

data SpectrumData = SpectrumData
    { _spectrumQPosition :: [Double]
    , _spectrumDistance :: Double
    , _spectrumBand :: Vector DataBand
    } deriving (Eq, Show, Read)

$(let f "_spectrumQPosition" = "q-position"
      f "_spectrumDistance" = "distance"
      f "_spectrumBand" = "band"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''SpectrumData)
makeLenses ''SpectrumData

---------------------------------------------------------

data Point = Point
    { _pointSymbol :: String
    , _pointCoordinates :: [Double]
    , _pointMass :: Double
    } deriving (Eq, Show, Read)

$(let f "_pointSymbol" = "symbol"
      f "_pointCoordinates" = "coordinates"
      f "_pointMass" = "mass"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''Point)
makeLenses ''Point

---------------------------------------------------------

data BandYaml = BandYaml
    { _bandYamlNQPoint :: Int
    , _bandYamlNPath   :: Int
    , _bandYamlSegmentNQPoint :: [Int]
    , _bandYamlReciprocalLattice :: [[Double]]
    , _bandYamlNAtom :: Int
    , _bandYamlLattice :: [[Double]]
    , _bandYamlPoints :: Vector Point
    , _bandYamlSupercellMatrix :: [[Double]]
    , _bandYamlSpectrum :: Vector SpectrumData
    } deriving (Eq, Show, Read)

$(let f "_bandYamlNQPoint"           = "nqpoint"
      f "_bandYamlNPath"             = "npath"
      f "_bandYamlSegmentNQPoint"    = "segment_nqpoint"
      f "_bandYamlReciprocalLattice" = "reciprocal_lattice"
      f "_bandYamlNAtom"             = "natom"
      f "_bandYamlLattice"           = "lattice"
      f "_bandYamlPoints"            = "points"
      f "_bandYamlSupercellMatrix"   = "supercell_matrix"
      f "_bandYamlSpectrum"          = "phonon"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''BandYaml)
makeLenses ''BandYaml
