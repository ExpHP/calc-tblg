{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Band.Fold(
    foldBandComputation,
    testSuite_49db2cfe_158e_40ac_9399_1d5a07ffae96,
    ) where

import           "exphp-prelude" ExpHPrelude hiding (putStr, transpose)
import           "base" Data.Monoid(All(..))
import           "base" Data.Fixed(mod')
import           "base" Data.List.NonEmpty(NonEmpty)
import           "base" Data.Ratio(denominator)
import           "linear" Linear.V3
import           "linear" Linear.Matrix
import qualified "containers" Data.IntMap as IntMap

-- Test deps
import           "base" Data.Functor.Identity
import           GeneralUtil(reallyAssert,onlyUniqueValue)
import           TestUtil

type Q = Rational
type R = Double
type Z = Integer
type QVec a = V3 a
type IMat = M33 Z

-- TODO: Unit test

-- | Given two structures where the lattice of one is an exact superlattice
--   of the other, compute a band structure for a the larger structure by
--   folding bands from the smaller structure into the larger structure's FBZ.
--
-- ("fold" is used here in the physics sense; not the Haskell sense)
foldBandComputation :: (Integral n, Monad m, Real x, Fractional x)
                    => [[n]]              -- ^ row-based 3x3 supercell matrix C satisfying S = C A, where A and S
                                          --   are unit cells for the smaller and larger systems, respectively.
                                          --   (any missing entries are filled with the identity matrix)
                    -> ([[x]] -> m [[a]]) -- ^ callback to compute bands in the smaller system, taking fractional
                                          --   reciprocal-space coords and producing a list of values at each.
                    -> ([[x]] -> m [[a]]) -- ^ result is a similar function but for the larger system.
                                          --   Items within each inner list are arbitrarily ordered.
foldBandComputation cMat unitCompute qs = foldBandComputation_ cMat' unitCompute' qs'
  where
    cMat' = toM33 $ fmap (fmap fromIntegral) cMat
    unitCompute' = unitCompute . fmap (fmap realToFrac) . fmap toList
    qs' = fmap realToFrac . toV3 <$> qs

toV3 :: (Num a)=> [a] -> (V3 a)
toV3  [a]     = V3 a 0 0
toV3  [a,b]   = V3 a b 0
toV3  [a,b,c] = V3 a b c
toV3  _ = error "invalid 3D vector"
toM33 :: (Num a)=> [[a]] -> V3 (V3 a)
toM33 [a]     = V3 (toV3 a) (V3 0 1 0) (V3 0 0 1)
toM33 [a,b]   = V3 (toV3 a) (toV3 b)   (V3 0 0 1)
toM33 [a,b,c] = V3 (toV3 a) (toV3 b)   (toV3 c)
toM33 _ = error "invalid 3D matrix"

-- | Internal version using types from "linear".
--   Exposed for testing...
foldBandComputation_ :: (Monad m)
                     => IMat
                     -> ([QVec Q] -> m [[a]])
                     -> ([QVec Q] -> m [[a]])
foldBandComputation_ cMat unitCompute = superCompute
  where
    superCompute superQs = do
        let (unitLabels, unitQs) = unzip $ allImages (zip [0..] superQs)
        unitEs <- unitCompute unitQs
        let superEs = zip unitLabels unitEs
                    & IntMap.fromListWith (<>)    -- collect energies corresponding to same superQ.
                    & fmap snd . IntMap.toAscList -- order by superQs index
        let (Just perSuper) = onlyUniqueValue (fromIntegral . length <$> superEs)
        let (Just perUnit)  = onlyUniqueValue (fromIntegral . length <$> unitEs)
        let !() = reallyAssert (perUnit * det33 cMat == perSuper) ()
        pure superEs

    allImages :: [(label, QVec Q)] -> [(label, QVec Q)]
    allImages qs = [ (label, modBy' 1 <$> (gamma' + q)) | gamma' <- supercellGammas cMat
                                                        , (label, q) <- qs
                                                        ]

supercellGammas :: IMat     -- supercell matrix C (row-based) such that S = C A
                -> [QVec Q] -- fractional points in K_A which are images of Gamma in K_S
supercellGammas cMat =
    [ V3 ia ib ic | ia <- fromIntegral <$> (0 ..< na)
                  , ib <- fromIntegral <$> (0 ..< nb)
                  , ic <- fromIntegral <$> (0 ..< nc)
                  ] !*! kMat
  where
    kMat = inv33 (fmap fromIntegral <$> transpose cMat)
    V3 na nb nc = rationalMatrixPeriodsByBruteForce kMat

-- (you hear the author of the code muttering something unintelligible
--  about "infix operators" and "footguns"...)
modBy' :: (Real a)=> a -> a -> a
modBy' = flip Data.Fixed.mod'

infix 0 ..<
(..<) :: (Enum b)=> b -> b -> [b]
a ..< b = [a .. pred b]
------------------------------------------------------------------
------------------------------------------------------------------

testSuite_49db2cfe_158e_40ac_9399_1d5a07ffae96 :: TestTree
testSuite_49db2cfe_158e_40ac_9399_1d5a07ffae96 =
    "Subst" ~:
    [ "supercellGammas" ~:
        [ "trivial case" ~: supercellGammas identity @?= [V3 0 0 0]
        ]
    , "foldBandComputation_" ~:
        [ "quickCheck identity"  ~: qc propIdentity
        , "quickCheck identity_" ~: qc propIdentity_
        ]
    ]

-- make it easier to have quickcheck generate lists of equal length
type R7 = (R,R,R,R,R,R,R)
r7ToList :: R7 -> [R]
r7ToList (a,b,c,d,e,f,g) = [a,b,c,d,e,f,g]

propIdentity :: Fun (Q,Q,Q) (Identity R7) -> (NonEmpty (Q,Q,Q)) -> Property
propIdentity f qs =
    f' qs' === foldBandComputation [[1::Integer,0],[0,1]] f' qs'
  where
    f' = mapM (fmap r7ToList . apply f . (\[a,b,c] -> (a,b,c)))
    qs' = fmap (`mod'` 1) . (\(a,b,c) -> [a, b, c]) <$> toList qs

propIdentity_ :: Fun (Q,Q,Q) (Identity R7) -> (NonEmpty (Q,Q,Q)) -> Property
propIdentity_ f qs =
    f' qs' === foldBandComputation_ identity f' qs'
  where
    f' = mapM (fmap r7ToList . apply f . unV3)
    qs' = fmap (`mod'` 1) . enV3 <$> toList qs

type TestableV3 a = (a,a,a) -- V3 analogue with Arbitrary/Coarbitrary instances
-- type TestableM33 a = TestableV3 (TestableV3 a)

enV3 :: TestableV3 a -> V3 a
unV3 :: V3 a -> TestableV3 a
enV3 (a,b,c) = V3 a b c
unV3 (V3 a b c) = (a,b,c)

---------------------------------------------------

-- closely related to the brute force search for an HNF matrix.
-- | Given a rational matrix Q, get a row vector (nx ny nz)
--   such that the cubic volume spanned by (nx 0 0), (0 ny 0), and (0 0 nz)
--   contains a complete set of points with unique images under
--   multiplication by Q (modulo 1 elementwise).
rationalMatrixPeriodsByBruteForce :: M33 Q -> V3 Z
rationalMatrixPeriodsByBruteForce kMat@(V3 kx _ _) = V3 nx ny nz
  where
    -- we can pick the first lattice vector to make things easy;
    -- have it point directly along x
    nx = foldl lcm 1 $ denominator <$> kx

    -- the other lattice vectors may not necessarily be perpendicular.
    ny = head [ i | (i, kyMult) <- zip [1..] kyMultiples
                  ,     kxMult  <- take (fromIntegral nx) kxMultiples
                  , allIntegral (kxMult + kyMult)
                  ]

    nz = head [ i | (i, kzMult) <- zip [1..] kzMultiples
                  ,     kyMult  <- take (fromIntegral ny) kyMultiples
                  ,     kxMult  <- take (fromIntegral nx) kxMultiples
                  , allIntegral (kxMult + kyMult + kzMult)
                  ]

    allIntegral = getAll . foldMap (All . (1 ==) . denominator)
    multiples v = fmap (\c -> (c *) <$> v) [1..]
    V3 kxMultiples kyMultiples kzMultiples = multiples <$> kMat
