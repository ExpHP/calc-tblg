{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -Wall #-}

-----------------------------------------------------------------

module Phonopy.Types where

import           "exphp-prelude" ExpHPrelude hiding (transpose)
import qualified "containers" Data.Map as Map
import qualified "aeson" Data.Aeson as Aeson
import qualified "vector" Data.Vector as Vector
import           "linear" Linear.V3

import           Band.Aliases

-----------------------------------------------------------------

-- FIXME Pull this out and use it to fix all the hardcoded hsympath stuff in Shakefile-inner.hs
-- | A named point in reciprocal space.
type HSymPoint = String
data HighSymInfo = HighSymInfo
  { highSymInfoMap     :: Map HSymPoint QVec -- ^ Map associating HSymPoints with their locations.
  , highSymInfoHPoints :: [HSymPoint]        -- ^ High symmetry points to visit, in order.
  }


instance Aeson.FromJSON HighSymInfo where
    parseJSON = Aeson.withObject "highsym info" $ \o ->
        HighSymInfo <$> (fmap (\[a,b,c] -> V3 a b c) <$> o Aeson..: "point")
                    <*> o Aeson..: "path"

highSymInfoQPoints :: HighSymInfo -> [QVec]
highSymInfoQPoints HighSymInfo{..} = (highSymInfoMap Map.!) <$> toList highSymInfoHPoints
highSymInfoNLines :: HighSymInfo -> Int
highSymInfoNLines = (subtract 1) . length . highSymInfoHPoints

-----------------------------------------------------------------

-- data indexed by a qpoint on a high symmetry path
data QPathData a = QPathData
    { qPathDataByLine :: Vector (Vector a)
    } deriving (Eq,Show,Read)

type QPath = QPathData QVec

instance Functor QPathData where
    fmap f = QPathData . fmap (fmap f) . qPathDataByLine

newtype LineId = LineId Int
                 deriving (Eq, Ord, Show, Read)

qPathLineIds :: QPathData a -> [LineId]
qPathLineIds p = LineId <$> [0..length (qPathDataByLine p) - 1]
qPathLineLength :: QPathData a -> LineId -> Int
qPathLineLength p (LineId h) = length (qPathDataByLine p Vector.! h)
qPathAllIds :: QPathData a -> [(LineId, Int)]
qPathAllIds p = qPathLineIds p >>= \h -> (h,) <$> [0..qPathLineLength p h - 1]
qPathAt :: QPathData a -> (LineId, Int) -> a
qPathAt (QPathData p) (LineId h,i) = p Vector.! h Vector.! i

qPathsCompatible :: QPathData a -> QPathData b -> Bool
qPathsCompatible p q = (==) (qPathLineLength p <$> qPathLineIds p)
                            (qPathLineLength q <$> qPathLineIds q)

-- | Make a QPath from the major points visited, and the number of points along each line.
--   (There should be one less length than the number of points!)
--
--   Attempts to faithfully reproduce the same set of points that phonopy would in edge cases.
mkQPath :: [V3 Double] -> [Int] -> QPath
mkQPath points lengths
    | length points /= length lengths + 1 = error "mkHSymPath: incompatible band/points lengths"
    | otherwise = result
  where
    pointsV = Vector.fromList points
    lengthsV = Vector.fromList lengths
    qpointsByLineV :: Vector (Vector QVec)
    qpointsByLineV =
        fmap (\(V3 as bs cs) -> Vector.zipWith3 V3 as bs cs)
        [ phonopyLinspaceV n <$> q1 <*> q2 | n <- lengthsV
                                           | q1 <- Vector.init pointsV
                                           | q2 <- Vector.tail pointsV
                                           ]
    phonopyLinspaceV n a b = Vector.fromList $ linspace n a b

    result = QPathData qpointsByLineV

    -- NOTE: consistent with how phonopy does Q Paths, this function:
    --  * Will just yield [a] if n == 1.
    --  * Will include both endpoints otherwise.
    linspace :: (Fractional a)=> Int -> a -> a -> [a]
    linspace n a b = linspaceItem n a b <$> [0..n-1]

    -- Single item of a linspace
    linspaceItem :: (Fractional a)=> Int -> a -> a -> Int -> a
    linspaceItem 1 a _ 0 = a
    linspaceItem n a b k = let n' = realToFrac n
                               k' = realToFrac k
                           in (a * (n'-1-k') + b * k') / (n'-1)
