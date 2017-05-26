{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Band.Fold(
    foldBandComputation,
    unfoldBandComputation,
    testSuite_49db2cfe_158e_40ac_9399_1d5a07ffae96,
    ) where

import           "exphp-prelude" ExpHPrelude hiding (putStr, transpose)
import           "base" Data.Monoid(All(..))
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
--   of the other, compute a band structure for a the LARGER structure by
--   folding bands from the SMALLER structure into the larger structure's FBZ.
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

-- | Given two structures where the lattice of one is an exact superlattice
--   of the other, compute a band structure for a the SMALLER structure by
--   unfolding bands from the LARGER structure into the smaller structure's FBZ.
--
-- ("fold" is used here in the physics sense; not the Haskell sense)
unfoldBandComputation :: (Integral n, Monad m, Real x, Fractional x)
                    => [[n]]              -- ^ row-based 3x3 supercell matrix C satisfying S = C A, where A and S
                                          --   are unit cells for the smaller and larger systems, respectively.
                                          --   (any missing entries are filled with the identity matrix)
                    -> ([[x]] -> m [[a]]) -- ^ callback to compute bands in the smaller system, taking fractional
                                          --   reciprocal-space coords and producing a list of values at each.
                    -> ([[x]] -> m [[a]]) -- ^ result is a similar function but for the larger system.
                                          --   Items within each inner list are arbitrarily ordered.
unfoldBandComputation cMat superCompute qs = foldBandComputation_ cMat' superCompute' qs'
  where
    cMat' = toM33 $ fmap (fmap fromIntegral) cMat
    superCompute' = superCompute . fmap (fmap realToFrac) . fmap toList
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
        let !() = reallyAssert (perUnit * volume cMat == perSuper) ()
        pure superEs

    allImages :: [(label, QVec Q)] -> [(label, QVec Q)]
    allImages qs = [ (label, mod1 <$> (gamma' + (q *! kMat))) | gamma' <- supercellGammas cMat
                                                              , (label, q) <- qs
                                                              ]
    kMat = inv33 . transpose . fmap (fmap fromIntegral) $ cMat

-- | Internal version using types from "linear".
unfoldBandComputation_ :: (Monad m)
                       => IMat
                       -> ([QVec Q] -> m [[a]])
                       -> ([QVec Q] -> m [[a]])
unfoldBandComputation_ cMat superCompute = unitCompute
  where
    unitCompute unitQs = superCompute $ fmap mod1 <$> (unitQs !*! kInv)
    kInv = transpose . fmap (fmap fromIntegral) $ cMat

supercellGammas :: IMat     -- supercell matrix C (row-based) such that S = C A
                -> [QVec Q] -- fractional points in K_A which are images of Gamma in K_S
supercellGammas cMat =
    [ V3 ia ib ic | ia <- fromIntegral <$> (0 ..< na)
                  , ib <- fromIntegral <$> (0 ..< nb)
                  , ic <- fromIntegral <$> (0 ..< nc)
                  ] !*! kMat
  where
    kMat = inv33 . transpose . fmap (fmap fromIntegral) $ cMat
    V3 na nb nc = rationalMatrixPeriods kMat

mod1 :: (RealFrac a)=> a -> a
mod1 x = x - fromInteger (floor x)

-- stop forgetting to take the absolute value!
volume :: M33 Z -> Z
volume = abs . det33

infix 0 ..<
(..<) :: (Enum b)=> b -> b -> [b]
a ..< b = [a .. pred b]

------------------------------------------------------------------
------------------------------------------------------------------

-- make it easier to have quickcheck generate lists of equal length
type R7 = (R,R,R,R,R)
type W3  = (Word8,Word8,Word8)
type W33 = (W3,W3,W3)
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c
i33ToM33 :: W33 -> M33 Z
i33ToM33 = fmap (fmap (fromIntegral . (`mod` 16))) . fmap (uncurry3 V3) . uncurry3 V3
r7ToList :: R7 -> [R]
r7ToList (a,b,c,d,e) = [a,b,c,d,e]

type TestableV3 a = (a,a,a) -- V3 analogue with Arbitrary/Coarbitrary instances
-- type TestableM33 a = TestableV3 (TestableV3 a)

enV3 :: TestableV3 a -> V3 a
unV3 :: V3 a -> TestableV3 a
enV3 (a,b,c) = V3 a b c
unV3 (V3 a b c) = (a,b,c)

-- FIXME these tests are ungodly slow. Profiling suggests that most of the time
--         is spent in constructing Fun objects
testSuite_49db2cfe_158e_40ac_9399_1d5a07ffae96 :: TestTree
testSuite_49db2cfe_158e_40ac_9399_1d5a07ffae96 =
    -- FIXME the identity matrix doesn't exactly make for a good regression test.
    "Subst" ~:

    [ "identity matrix" ~:
        [ "supercellGammas" ~: supercellGammas identity @?= [V3 0 0 0]
        , "foldBandComputation"    ~: qc propIdentityFold
        , "foldBandComputation_"   ~: qc propIdentityFold_
        , "unfoldBandComputation"  ~: qc propIdentityUnfold
        , "unfoldBandComputation_" ~: qc propIdentityUnfold_
        ]

    -- FIXME too slow
--    , "relationships" ~:
--        [ "unfold-then-fold" ~: qc propUnfoldFold -- FIXME too slow.
--        ] :: [TestTree]

    ]
  where
    propIdentityFold    :: Fun (Q,Q,Q) (Identity R7) -> (NonEmpty (Q,Q,Q)) -> Property
    propIdentityFold_   :: Fun (Q,Q,Q) (Identity R7) -> (NonEmpty (Q,Q,Q)) -> Property
    propIdentityUnfold  :: Fun (Q,Q,Q) (Identity R7) -> (NonEmpty (Q,Q,Q)) -> Property
    propIdentityUnfold_ :: Fun (Q,Q,Q) (Identity R7) -> (NonEmpty (Q,Q,Q)) -> Property

    propIdentityFold    f qs = (getF  f) (getQs  qs) === foldBandComputation    eye  (getF  f) (getQs  qs)
    propIdentityFold_   f qs = (getF_ f) (getQs_ qs) === foldBandComputation_   eye_ (getF_ f) (getQs_ qs)
    propIdentityUnfold  f qs = (getF  f) (getQs  qs) === unfoldBandComputation  eye  (getF  f) (getQs  qs)
    propIdentityUnfold_ f qs = (getF_ f) (getQs_ qs) === unfoldBandComputation_ eye_ (getF_ f) (getQs_ qs)

    propUnfoldFold :: W33 -> Fun (Q,Q,Q) (Identity R7) -> (NonEmpty (Q,Q,Q)) -> Property
    propUnfoldFold cMat f qs =
        let (c', f', qs') = (getC_ cMat, getF_ f, take 20 $ getQs_ qs) in
        acceptableCmat c' ==> -- traceShow (volume c', (show f), length qs) $
            ((===) `on` fmap (fmap sort)) (fmap (cycleNTimes (fromIntegral $ volume c')) <$> f' qs')
                                          (foldBandComputation_ c' (unfoldBandComputation_ c' f') qs')

    cycleNTimes n xs = take (length xs * n) (cycle xs)

    acceptableCmat c = (volume c /= 0) && (volume c < 1000)
    eye = [[1::Integer, 0], [0, 1]]
    eye_ = identity
    getF  f  = mapM (fmap r7ToList . apply f . (\[a,b,c] -> (a,b,c)))
    getQs qs = fmap mod1 . (\(a,b,c) -> [a, b, c]) <$> toList qs
    getF_  f  = mapM (fmap r7ToList . apply f . unV3)
    getQs_ qs = fmap mod1 . enV3 <$> toList qs
    getC_ = i33ToM33

---------------------------------------------------

-- closely related to the brute force search for an HNF matrix.
-- | Given a rational matrix Q, get a row vector (nx ny nz)
--   such that the cubic volume spanned by (nx 0 0), (0 ny 0), and (0 0 nz)
--   contains a complete set of points with unique images under
--   multiplication by Q (modulo 1 elementwise).
rationalMatrixPeriods :: M33 Q -> V3 Z
rationalMatrixPeriods kMat = diagonal $ bruteForceHnfFromPredicate (allIntegral . (*! kMat) . fmap fromIntegral)
  where allIntegral = getAll . foldMap (All . (1 ==) . denominator)

bruteForceHnfFromPredicate :: (V3 Z -> Bool) -> M33 Z
bruteForceHnfFromPredicate p = V3 row1 row2 row3
  where
    search = head . filter p
    row1@(V3 na 0 0) = search [ V3 a 0 0 | a <- [1..]
                                         ]

    row2@(V3 _ nb 0) = search [ V3 a b 0 | b <- [1..]
                                         , a <- (0 ..< na)
                                         ]

    row3@(V3 _ _ _c) = search [ V3 a b c | c <- [1..]
                                         , b <- (0 ..< nb)
                                         , a <- (0 ..< na)
                                         ]
