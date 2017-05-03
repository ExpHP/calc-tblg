{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Band.Fold(foldBandComputation) where

import           "exphp-prelude" ExpHPrelude hiding (putStr, transpose)
import           "base" Data.Fixed(mod')
import           "linear" Linear.V3
import           "linear" Linear.Matrix
import qualified "containers" Data.IntMap as IntMap

type QVec = V3 Double
type IMat = M33 Int

-- | Given two structures where the lattice of one is an exact superlattice
--   of the other, compute a band structure for a the larger structure by
--   folding bands from the smaller structure into the larger structure's FBZ.
--
-- ("fold" is used here in the physics sense; not the Haskell sense)
foldBandComputation :: (Integral n, Monad m)
                    => [[n]]                   -- ^ row-based 3x3 supercell matrix C satisfying S = C A, where A and S
                                               --   are unit cells for the smaller and larger systems, respectively.
                                               --   (any missing entries are filled with the identity matrix)
                    -> ([[Double]] -> m [[a]]) -- ^ callback to compute bands in the smaller system, taking fractional
                                               --   reciprocal-space coords and producing a list of values at each.
                    -> ([[Double]] -> m [[a]]) -- ^ result is a similar function but for the larger system.
                                               --   Items within each inner list are arbitrarily ordered.
foldBandComputation cMat unitCompute qs = impl cMat' unitCompute' qs'
  where
    cMat' = toM33 $ fmap (fmap fromIntegral) cMat
    unitCompute' = unitCompute . fmap toList
    qs' = toV3 <$> qs

    toV3  [a]     = V3 a 0 0
    toV3  [a,b]   = V3 a b 0
    toV3  [a,b,c] = V3 a b c
    toV3  _ = error "invalid 3D vector"
    toM33 [a]     = V3 (toV3 a) (V3 0 1 0) (V3 0 0 1)
    toM33 [a,b]   = V3 (toV3 a) (toV3 b)   (V3 0 0 1)
    toM33 [a,b,c] = V3 (toV3 a) (toV3 b)   (toV3 c)
    toM33 _ = error "invalid 3D matrix"

impl :: (Monad m)
     => IMat                -- ^ row-based supercell matrix C satisfying S = C A, where A and S
                            --   are unit cells for the smaller and larger systems, respectively.
     -> ([QVec] -> m [[a]]) -- ^ callback to compute bands in the smaller system, taking fractional
                            --   reciprocal-space coords and producing a list of values at each.
     -> ([QVec] -> m [[a]]) -- ^ result is a similar function but for the larger system.
                            --   Items within each inner list are arbitrarily ordered.
impl cMat unitCompute = superCompute
  where
    superCompute superQs = do
        let (unitLabels, unitQs) = unzip $ allImages (zip [0..] superQs)
        unitEs <- unitCompute unitQs
        zip unitLabels unitEs
            & IntMap.fromListWith (<>)    -- collect energies corresponding to same superQ.
            & fmap snd . IntMap.toAscList -- order by superQs index
            & pure

    allImages :: [(label, QVec)] -> [(label, QVec)]
    allImages qs = [ (label, modBy' 1 <$> (gamma' + q)) | gamma' <- supercellGammas cMat
                                                        , (label, q) <- qs
                                                        ]

supercellGammas :: M33 Int    -- supercell matrix C (row-based) such that S = C A
                -> [QVec]     -- fractional points in K_A which are images of Gamma in K_S
supercellGammas cMat =
    let (V3 a b c) = diagonal cMat
    in [ V3 i j k | i <- fromIntegral <$> [0 .. pred a]
                  , j <- fromIntegral <$> [0 .. pred b]
                  , k <- fromIntegral <$> [0 .. pred c]
                  ] !*! inv33 (fmap fromIntegral <$> transpose cMat)

-- (you hear the author of the code muttering something unintelligible
--  about "infix operators" and "footguns"...)
modBy' :: (Real a)=> a -> a -> a
modBy' = flip Data.Fixed.mod'
