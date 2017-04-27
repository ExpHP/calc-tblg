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

-- !!! NOTE: This module has been superceded by Oracle.Phonopy.BandYaml.LL1 !!!
-- (fromJSON is not a streaming interface, and uses far more memory than we can handle)

-- Raw types for parsing band.yaml,
-- with straightforward ToJSON/FromJSON implementations.

module Band.Oracle.Phonopy.BandYaml where

import           "base" Data.Complex
import           "exphp-prelude" ExpHPrelude
import           "aeson" Data.Aeson
import           "aeson" Data.Aeson.Types
import           "aeson" Data.Aeson.TH
import qualified "vector" Data.Vector as Vector
import qualified "unordered-containers" Data.HashMap.Strict as HashMap

---------------------------------------------------------

data DataBand = DataBand
    { bandFrequency :: Double
    , bandEigenvector :: Maybe (Vector (Vector (Double, Double)))
    } deriving (Eq, Show, Read)

$(let f "bandFrequency" = "frequency"
      f "bandEigenvector" = "eigenvector"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''DataBand)

---------------------------------------------------------

data SpectrumData = SpectrumData
    { spectrumQPosition :: [Double]
    , spectrumDistance :: Double
    , spectrumBand :: Vector DataBand
    } deriving (Eq, Show, Read)

$(let f "spectrumQPosition" = "q-position"
      f "spectrumDistance" = "distance"
      f "spectrumBand" = "band"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''SpectrumData)

---------------------------------------------------------

data Point = Point
    { pointSymbol :: String
    , pointCoordinates :: [Double]
    , pointMass :: Double
    } deriving (Eq, Show, Read)

$(let f "pointSymbol" = "symbol"
      f "pointCoordinates" = "coordinates"
      f "pointMass" = "mass"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''Point)

---------------------------------------------------------

data BandYaml = BandYaml
    { bandYamlNQPoint :: Int
    , bandYamlNPath   :: Int
    , bandYamlSegmentNQPoint :: [Int]
    , bandYamlReciprocalLattice :: [[Double]]
    , bandYamlNAtom :: Int
    , bandYamlLattice :: [[Double]]
    , bandYamlPoints :: Vector Point
    , bandYamlSupercellMatrix :: [[Double]]
    , bandYamlSpectrum :: Vector SpectrumData
    } deriving (Eq, Show, Read)

$(let f "bandYamlNQPoint"           = "nqpoint"
      f "bandYamlNPath"             = "npath"
      f "bandYamlSegmentNQPoint"    = "segment_nqpoint"
      f "bandYamlReciprocalLattice" = "reciprocal_lattice"
      f "bandYamlNAtom"             = "natom"
      f "bandYamlLattice"           = "lattice"
      f "bandYamlPoints"            = "points"
      f "bandYamlSupercellMatrix"   = "supercell_matrix"
      f "bandYamlSpectrum"          = "phonon"
  in deriveJSON defaultOptions { fieldLabelModifier = f } ''BandYaml)

---------------------------------------------------------
-- Actually, the above types are a terrible idea once the files start
-- getting a few hundred megabytes big.

-- Helpers to deal with 'Parser -> Value a', a nested applicative.
type ParseFunc a = Value -> Parser a
infixl 4 <<$>>, <<*>>
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = (<$>) . (<$>)
(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = (<*>) . fmap (<*>)
ppure :: (Applicative f, Applicative g) => a -> f (g a)
ppure = pure . pure

parseComplex :: ParseFunc (Complex Double)
parseComplex = (:+) <<$>> parseJSON <<*>> parseJSON

parseVector :: ParseFunc a -> ParseFunc (Vector a)
parseVector f = withArray "parseVector" (Vector.mapM f)

onKey :: Text -> ParseFunc a -> ParseFunc a
onKey key p = withObject "parseKey" $ \o -> p (o HashMap.! key)

parseKet :: ParseFunc (UVector (Complex Double))
parseKet = onKey "eigenvector" $ Vector.convert . (>>= id) <<$>> parseVector (parseVector parseComplex)

parseKets :: ParseFunc (Vector (UVector (Complex Double)))
parseKets = onKey "band" $ parseVector parseKet

parseEigenvectors :: ParseFunc (Vector (Vector (UVector (Complex Double))))
parseEigenvectors = onKey "phonon" $ parseVector parseKets
