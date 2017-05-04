{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Band.FoldTest
    ( testSuite
    ) where

import           Band.Fold
import           "base" Data.Functor.Identity
import           "base" Data.Fixed(mod')
import           "linear" Linear.Matrix
import           "linear" Linear.V3
import           TestUtils

testSuite :: TestTree
testSuite =
    "Subst" ~:
    [ "supercellGammas" ~:
        [ "trivial case" ~: supercellGammas identity @?= [V3 0 0 0]
        ]
    , "foldBandComputation_" ~:
        [ "quickCheck identity"  ~: qc propIdentity
        , "quickCheck identity_" ~: qc propIdentity_
        ]
    ]

propIdentity :: Fun (Double,Double,Double) (Identity [Double]) -> [(Double,Double,Double)] -> Property
propIdentity f qs =
    f' qs' === foldBandComputation [[1::Integer,0],[0,1]] f' qs'
  where
    f' = mapM (apply f . (\[a,b,c] -> (a,b,c)))
    qs' = fmap (`mod'` 1) . (\(a,b,c) -> [a, b, c]) <$> qs

propIdentity_ :: Fun (Double,Double,Double) (Identity [Double]) -> [(Double,Double,Double)] -> Property
propIdentity_ f qs =
    f' qs' === foldBandComputation_ identity f' qs'
  where
    f' = mapM (apply f . unV3)
    qs' = fmap (`mod'` 1) . enV3 <$> qs

type TestableV3 a = (a,a,a) -- V3 analogue with Arbitrary/Coarbitrary instances
-- type TestableM33 a = TestableV3 (TestableV3 a)

enV3 :: TestableV3 a -> V3 a
unV3 :: V3 a -> TestableV3 a
enV3 (a,b,c) = V3 a b c
unV3 (V3 a b c) = (a,b,c)
