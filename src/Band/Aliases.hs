
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_GHC -Wall #-}

-- This module shouldn't exist.
--
-- Had I the ability to mark something as private, I would rather declare these individually
--  in each module that uses them.
--
-- Newly written modules should not feel obliged to use these types for their internal data
-- structures if another type would be more natural.  However, it may be good to use these
-- types in code that interfaces with modules that use these types (since in those cases,
-- the coupling is already present)


module Band.Aliases where

import           "exphp-prelude" ExpHPrelude
import           "base" Data.Complex
import           "vector" Data.Vector(Vector)
import           "linear" Linear.V3

type Ket = UVector (Complex Double)
type Kets = Vector Ket
type Perm = Vector Int
type Energies = Vector Double
type QVec = V3 Double
