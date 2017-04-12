{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Band.BandYaml where

import           "vector" Data.Vector(Vector)
import           "aeson" Data.Aeson

---------------------------------------------------------

(.::) :: _ => _ -> _ -> _
(.::) = (.:)

(?::) :: _ => _ -> _ -> _
(?::) = (.:?)

(==>) :: _ => _ -> _ -> _
value ==> label = [label .= value]

(?=>) :: _ => Maybe _ -> _ -> _
(Just value) ?=> label = [label .= value]
Nothing      ?=> _     = []

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
    }

instance FromJSON BandYaml where
    parseJSON = withObject "band.yaml" $ \o -> do
        bandYamlNQPoint            <- o .:: "nqpoint"
        bandYamlNPath              <- o .:: "npath"
        bandYamlSegmentNQPoint     <- o .:: "segment_nqpoint"
        bandYamlReciprocalLattice  <- o .:: "reciprocal_lattice"
        bandYamlNAtom              <- o .:: "natom"
        bandYamlLattice            <- o .:: "lattice"
        bandYamlPoints             <- o .:: "points"
        bandYamlSupercellMatrix    <- o .:: "supercell_matrix"
        bandYamlSpectrum           <- o .:: "phonon"
        pure BandYaml{..}

instance ToJSON BandYaml where
    toJSON BandYaml{..} = object . concat $
        [ bandYamlNQPoint               ==> "nqpoint"
        , bandYamlNPath                 ==> "npath"
        , bandYamlSegmentNQPoint        ==> "segment_nqpoint"
        , bandYamlReciprocalLattice     ==> "reciprocal_lattice"
        , bandYamlNAtom                 ==> "natom"
        , bandYamlLattice               ==> "lattice"
        , bandYamlPoints                ==> "points"
        , bandYamlSupercellMatrix       ==> "supercell_matrix"
        , bandYamlSpectrum              ==> "phonon"
        ]

---------------------------------------------------------

data Point = Point
    { pointSymbol :: String
    , pointCoordinates :: [Double]
    , pointMass :: Double
    }

instance FromJSON Point where
    parseJSON = withObject "point object" $ \o -> do
        pointSymbol         <- o .:: "symbol"
        pointCoordinates    <- o .:: "coordinates"
        pointMass           <- o .:: "mass"
        pure Point{..}

instance ToJSON Point where
    toJSON Point{..} = object . concat $
        [ pointSymbol            ==> "symbol"
        , pointCoordinates       ==> "coordinates"
        , pointMass              ==> "mass"
        ]

---------------------------------------------------------

data SpectrumData = SpectrumData
    { spectrumQPosition :: [Double]
    , spectrumDistance :: Double
    , spectrumBand :: Vector DataBand
    }

instance FromJSON SpectrumData where
    parseJSON = withObject "SpectrumData" $ \o -> do
        spectrumQPosition    <- o .:: "q-position"
        spectrumDistance     <- o .:: "distance"
        spectrumBand         <- o .:: "band"
        pure SpectrumData{..}

instance ToJSON SpectrumData where
    toJSON SpectrumData{..} = object . concat $
        [ spectrumQPosition       ==> "q-position"
        , spectrumDistance        ==> "distance"
        , spectrumBand            ==> "band"
        ]

---------------------------------------------------------

data DataBand = DataBand
    { bandFrequency :: Double
    , bandEigenvector :: Maybe (Vector (Vector (Double, Double)))
    }

instance FromJSON DataBand where
    parseJSON = withObject "DataBand" $ \o -> do
        bandFrequency        <- o .:: "frequency"
        bandEigenvector      <- o ?:: "eigenvector"
        pure DataBand{..}

instance ToJSON DataBand where
    toJSON DataBand{..} = object . concat $
        [ bandFrequency           ==> "frequency"
        , bandEigenvector         ?=> "eigenvector"
        ]
