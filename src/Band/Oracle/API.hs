
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -Wall #-}

module Band.Oracle.API where

import           "base" Data.Complex
import           "vector" Data.Vector(Vector)

type Ket = Vector (Complex Double)
type Kets = Vector Ket
type Perm = Vector Int
type Energies = Vector Double

newtype LineId = LineId Int
                 deriving (Eq, Ord, Show, Read)
