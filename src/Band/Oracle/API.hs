
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}

module Band.Oracle.API where

import           "exphp-prelude" ExpHPrelude
import           "base" Data.Complex
import           "vector" Data.Vector(Vector)

type Ket = UVector (Complex Double)
type Kets = Vector Ket
type Perm = Vector Int
type Energies = Vector Double

newtype LineId = LineId Int
                 deriving (Eq, Ord, Show, Read)
