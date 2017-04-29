{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- these helper functions ought to be defined right next to where they are used,
-- but cannot be without me having to add massive export lists to every module
-- just to hide them and prevent ambiguous name errors.
-- Thanks, Haskell.

module GeneralUtil where

import           "base" Data.Foldable
import qualified "base" Data.List as List

-- helps debug type errors
tc :: () -> ()
tc = id

-- fffffunctions for fffffunctors
ffmap :: (_) => (a -> b) -> s (t a) -> s (t b)
ffmap = fmap . fmap
fffmap :: (_) => (a -> b) -> s (t (u a)) -> s (t (u b))
fffmap = fmap . ffmap
ffffmap :: (_) => (a -> b) -> s (t (u (v a))) -> s (t (u (v b)))
ffffmap = fmap . fffmap

-- folds for validating redundant data
onlyValue :: (Foldable t)=> t a -> Maybe a
onlyValue xs = case toList xs of [x] -> Just x
                                 _   -> Nothing

bool :: a -> a -> Bool -> a
bool a _ False = a
bool _ b True  = b

boolM :: (Functor m)=> a -> a -> m Bool -> m a
boolM a b m = bool a b <$> m

onlyUniqueValue :: (Eq a, Foldable t)=> t a -> Maybe a
onlyUniqueValue = onlyValue . List.nub . toList -- This usage of nub is O(n) in the common case.

expect :: String -> Maybe a -> a
expect msg = maybe (error msg) id

reallyAssert :: Bool -> a -> a
reallyAssert False = error "reallyAssert: fail"
reallyAssert True  = id

assertSorted :: (Ord a)=> [a] -> [a]
assertSorted (x0:xs@(x1:_)) | x0 > x1 = error "assertSorted: it isn't"
                            | otherwise = x0 : assertSorted xs
assertSorted xs = xs
