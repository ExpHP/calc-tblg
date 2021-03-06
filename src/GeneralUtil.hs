{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

-- these helper functions ought to be defined right next to where they are used,
-- but cannot be without me having to add massive export lists to every module
-- just to hide them and prevent ambiguous name errors.
-- Thanks, Haskell.

module GeneralUtil(
    tc,
    onlyValue, onlyUniqueValue,
    bool, boolM,
    expect, reallyAssert, assertSorted,
    edivMod, ediv, emod,
    testSuite_c2b7f613_b9d9_46ee_a252_78f5644ade15,
    ) where

import           "base" Data.Foldable
import qualified "base" Data.List as List

import           TestUtil

----------------------------------------------
-- helps debug type errors

tc :: () -> ()
tc = id

----------------------------------------------
-- folds for validating redundant data

onlyValue :: (Foldable t)=> t a -> Maybe a
onlyValue xs = case toList xs of [x] -> Just x
                                 _   -> Nothing

onlyUniqueValue :: (Eq a, Foldable t)=> t a -> Maybe a
onlyUniqueValue = onlyValue . List.nub . toList -- This usage of nub is O(n) in the common case.

----------------------------------------------
-- case matching on booleans

bool :: a -> a -> Bool -> a
bool a _ False = a
bool _ b True  = b

boolM :: (Functor m)=> a -> a -> m Bool -> m a
boolM a b = fmap (bool a b)

----------------------------------------------

expect :: String -> Maybe a -> a
expect msg = maybe (error msg) id

reallyAssert :: Bool -> a -> a
reallyAssert False = error "reallyAssert: fail"
reallyAssert True  = id

assertSorted :: (Ord a)=> [a] -> [a]
assertSorted (x0:xs@(x1:_)) | x0 > x1 = error "assertSorted: it isn't"
                            | otherwise = x0 : assertSorted xs
assertSorted xs = xs

---------------------------------------------------------------------------------
edivMod :: (Integral a)=> a -> a -> (a,a)
edivMod a b = case a `quotRem` b of (d, r) | r < 0     -> (d - signum b, r + abs b)
                                           | otherwise -> (d, r)

ediv :: (Integral a)=> a -> a -> a
ediv a b = fst $ edivMod a b
emod :: (Integral a)=> a -> a -> a
emod a b = snd $ edivMod a b

----------------------------------------------

testSuite_c2b7f613_b9d9_46ee_a252_78f5644ade15 :: TestTree
testSuite_c2b7f613_b9d9_46ee_a252_78f5644ade15 =
    "GeneralUtil tests" ~:
    [ "edivMod" ~: edivModTests
    ]

edivModTests :: [TestTree]
edivModTests =
    -- NOTE: these properties are enough to fully specify the output;
    --       it is just up to quickcheck to provide complete enough coverage
    [ "QC range of modulus" ~: qc propModulus
    , "QC div/mod relation" ~: qc propDivModSum
    , "QC div, mod, divMod" ~: qc propDivModPair
    ]
  where
    propModulus    :: Integer -> Integer -> Property
    propModulus    a b = (b /= 0) ==> let m = a `emod` b in 0 <= m && m < abs b
    propDivModSum  :: Integer -> Integer -> Property
    propDivModSum  a b = (b /= 0) ==> let (d,m) = a `edivMod` b in a === b*d + m
    propDivModPair :: Integer -> Integer -> Property
    propDivModPair a b = (b /= 0) ==> (a `ediv` b, a `emod` b) === a `edivMod` b
