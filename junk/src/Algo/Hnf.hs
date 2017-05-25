
-- JUNKED FILE NOTES
-- -----------------

-- WHAT HAPPENED IN THE END? (WHAT CAN WE LEARN)
--
-- I eventually realized that all I really wanted were the diagonal elements of
-- the HNF matrix, and ended up writing a simple brute force search for them
-- that used a lattice-point test predicate.
--
-- Consider if this suffices for your use case, and if so,
-- gaze no further upon this file. You will live a far happier life.

-- WHAT HAPPENED HERE?
--
-- I wasted a week and a half on this and was never ever happy.
-- There were many redesigns, including:
--
-- * manual implementation based on linear
-- * manual implementation based on bed-and-breakfast
-- * implementation based on algo in np-linear

-- WHAT MADE IT SO TROUBLESOME
--
-- I could not satisfy all of my requirements:
--
-- * Support for my use case (small, 3x3 invertible matrices)
-- * A test suite I am pleased with
--   (hard to be pleased with it when you add restrictions like invertibility
--    that require 5 pounds of wrapper types and Arbitrary instances)
-- * Exact computation of the unimodular matrix and its inverse.
--   (when restricted to invertible input matrices, you can use libraries like linear
--    or bed-and-breakfast which have exact inverses to derive these after the fact;
--    otherwise, you need to make these computations part of the algorithm!)
--   (worse yet, if you DO make them part of the algorithm, then there's an abstraction hazard;
--    it's tempting to try to abstract operations over all three matrices so that they
--    can all be treated by the same logic, but it's unclear what a sufficient set of
--    operations would be. (I made many wrong guesses here))

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Algo.Hnf(
    HnfDecomp,
    HnfDecomp'(..),
    hnfDecomp,

    RowOrColumnCentric(..),
    TriangularForm(..),
    flipForm,

    testSuite_0b3c9ae2_c8e6_4858_aa87_f6b0381afb6b,
    ) where

-- FIXME: ugh, another catastrophe
--
-- This exposes a seemingly-general interface which is actually a lie; it only computes
--  upper-triangular row-based HNFs, and uses methods which are only suitable for very
--  small matrices.
--
-- I believe there are three forces at work which caused this module to come out like this:
--
-- - My sole use-case is small (usually 3x3) invertible matrices,
--   and invertible matrices can be handled *far* more simply than the general case.
-- - It is easy to make mistakes in this implementation, so I want to test it
-- - QuickCheck's design encourages one to write general algorithms that hit all edge cases

import           "exphp-prelude" ExpHPrelude hiding (putStr,unwords)

import qualified Algo.Hnf.Impl2 as Impl

import           TestUtil
import           "QuickCheck" Test.QuickCheck(Property,Arbitrary(..),choose,property,(===))
import qualified "QuickCheck" Test.QuickCheck.Gen as QuickCheck

type IMatrix = [[Integer]]

-- Note there are a variety of options for the HNF:
--
-- - Row-based (A = U A') or column-based (A = A' U)
-- - Upper-triangular or lower-triangular
--
-- Wikipedia includes upper-triangular as part of the definition of row-based,
-- but for low-dimensional row-based square matrices (e.g. supercell matrices) I find
-- lower-triangular to be more comfortable to reason about for geometric purposes.
-- (this is acheived by modifying the algorithm to start with the last column)

-- | Should the result be upper triangular or lower?
data TriangularForm = Lower | Upper deriving (Eq, Ord, Show, Read)
flipForm :: TriangularForm -> TriangularForm
flipForm Lower = Upper
flipForm Upper = Lower

-- | Are we using row addition or column addition to decompose the matrix?
--   NOTE: Currently only RowCentric is supported.
--         This argument really only exists to benefit the reader at the callsite.
data RowOrColumnCentric = RowCentric    -- ^ Decompose as A = U H (with U unimodular), using row addition
                        | ColumnCentric -- ^ Decompose as A = H U (with U unimodular), using column addition
                        deriving (Eq, Ord, Show, Read)

type HnfDecomp  b = HnfDecomp' [[b]]
data HnfDecomp' m = HnfDecomp
    { -- | The HNF form of the matrix.
    hnfForm :: m
    , -- | Get the unimodular transform U such that (U A' = A) (row-based)
      --   or (A' U = A) (column-based).
      --
      -- NOTE: The current implementation lazily computes this, and will perform
      --        a matrix inverse the first time you try to inspect this matrix.
      --       This is subject to change in the future, especially considering
      --        the fact that this method does not work in general for all
      --        matrices.
    hnfUnimodular :: m
    } deriving (Functor, Eq, Ord)

-- The relationship between the various forms of HNF can be described with two tools:
hnfDecomp :: (Integral b)=> TriangularForm -> RowOrColumnCentric -> [[b]] -> HnfDecomp b
hnfDecomp form rc = adjustForStyle form rc rowUpperSolve
  where
    -- Impl gives us the row-based upper triangular form
    rowUpperSolve = (HnfDecomp <$> Impl.rowUpperHnfPart <*> Impl.rowUpperUnimodularPart)
                    . Impl.semiHermite'

    -- working on the transpose lets us find
    adjustForStyle Upper RowCentric    = workOnIntegers
    adjustForStyle Lower RowCentric    = workOnIntegers . workOnReversal
    adjustForStyle Lower ColumnCentric = workOnIntegers . workOnTranspose
    adjustForStyle Upper ColumnCentric = workOnIntegers . workOnReversal . workOnTranspose

    workOnIntegers  f = fmap (fmap (fmap fromIntegral)) . f . fmap (fmap fromIntegral)
    workOnTranspose f = fmap transpose . f . transpose
    workOnReversal  f = fmap matrixReverse . f . matrixReverse

-- hnfDecomp :: (Integral b)=> Style -> [[b]] -> HnfDecomp b
-- hnfDecomp style = fmap (fmap (fmap fromIntegral) . Matrix.toList)
--                   . hnfDecomp' style . Matrix.fromList . fmap (fmap fromIntegral)

-- hnfDecomp' :: Style -> IMatrix -> HnfDecomp' IMatrix
-- hnfDecomp' style m | not (Matrix.isSquare m) = error "hnfDecomp: Currently restricted to invertible matrices."
--                    -- FIXME determinant = performance hazard... test diagonal entries of output instead?
--                    | Matrix.det m == 0       = error "hnfDecomp: Currently restricted to invertible matrices."
--                    | otherwise = hnfAlgo style m

toHnf :: TriangularForm -> RowOrColumnCentric -> IMatrix -> IMatrix
toHnf a b c = hnfForm (hnfDecomp a b c)

-- vdivMod :: Int -> IVector -> IVector -> (I, IVector)
-- vdivMod pivot a b | b ! pivot == 0 = error . P.unwords $ ["Division by 0:  vdivMod ", show pivot, "(" ++ show a ++ ")", "(" ++ show b ++ ")"]
-- vdivMod pivot a b = (d,m)
--   where
--     d = (a ! pivot) `ediv` (b ! pivot)
--     m = Vector.zipWith (-) a (fmap (d *) b)

-- vmod :: Int -> IVector -> IVector -> IVector
-- vmod pivot a b = snd $ vdivMod pivot a b

-- iterateUntilLeft :: (a -> Either b a) -> a -> b
-- iterateUntilLeft f a = case f a of Left b -> b
--                                    Right a' -> iterateUntilLeft f a'

-- gcdIter pivot (a,b) | traceShow (pivot, (a,b), gcdIter' pivot (a,b)) False = undefined
-- gcdIter pivot (a,b) = gcdIter' pivot (a,b)
-- gcdIter' :: Int -> (IVector, IVector) -> Either (IVector, IVector) (IVector, IVector)
-- gcdIter' pivot (a,b) | a ! pivot == 0 = Left (a,b)
-- --                    | abs (a ! pivot) > abs (b ! pivot) = Right (b, a)
--                     | otherwise      = Right (vmod pivot b a, a)

-- rowGcd :: Int -> (IVector, IVector) -> (IVector, IVector)
-- rowGcd k = iterateUntilLeft (gcdIter k)

-- -------------------------------------------------------------------------
-- -- NOTE: these assume the matrix is square and invertible

-- hnfAlgo :: Style -> IMatrix -> HnfDecomp' IMatrix
-- hnfAlgo (Style _    ColumnCentric) _ = error "hnfAlgo: ColumnCentric not supported"
-- hnfAlgo (Style form    RowCentric) m = (HnfDecomp <$> id <*> recoverUnimodularForInvertible)
--                                      . vectorsToMatrix
--                                      . doAllReduction form
--                                      . (id $!!)
--                                      . doAllSignCorrections form
--                                      . doAllGcds form
--                                      . matrixToVectors
--                                      $ m
--   where
--     -- restricting ourselves to invertible matrices makes it easy to recover the unimodular transform.
--     -- (Notice that if m is invertible, then so is its HNF)
--     recoverUnimodularForInvertible :: IMatrix -> IMatrix
--     recoverUnimodularForInvertible rowHnf =
--         Matrix.map toIntegralChecked
--               -- take advantage of bed-and-breakfast's exact inverses for rational matrices
--               (Matrix.map fromIntegral m / Matrix.map fromIntegral rowHnf :: Matrix Rational)

--     toIntegralChecked :: (Integral b)=> Rational -> b
--     toIntegralChecked r | denominator r == 1 = fromInteger $ numerator r
--                         | otherwise          = error $ "rationalToIntegralChecked: " ++ show r

-- doAllGcds :: TriangularForm -> IVectors -> IVectors
-- doAllGcds Lower m = foldr (.) id
--     -- For invertible matrices, the pivot for row k is simply k
--     [ doGcd k k i | k <- reverse (0 ..< length m)
--                   , i <- (0 ..< k)
--                   , m ! i ! k /= 0
--                   ] m
-- doAllGcds Upper _ = error "doAllGcds: upper triangular not yet supported"

-- doAllSignCorrections :: TriangularForm -> IVectors -> IVectors
-- doAllSignCorrections _ m = foldr (.) id
--     -- For invertible matrices, the pivot for row k is simply k
--     [ doSignCorrection k k | k <- (0 ..< length m) ] m

-- doAllReduction :: TriangularForm -> IVectors -> IVectors
-- doAllReduction Lower m = foldr (.) id
--     -- For invertible matrices, the pivot for row k is simply k
--     [ doReduce k k i | k <- (0 ..< length m)
--                      , i <- (k + 1 ..< length m)
--                      ] m
-- doAllReduction Upper _ = error "doAllReduction: upper triangular not yet supported"

-- -- NOTE: there's some pretty low-hanging fruit for optimization here by
-- -- changing this to do a few big (//) operations instead of many small ones.
-- -- (but doing this properly for doAllGcds requires some sort of stateful loop
-- --  to track the pivot row's contents, which I am too lazy to flesh out now)

-- -- looking at rows k and i and a given pivot column,
-- -- use row addition to:
-- --   - place the gcd (or its negation) in row k
-- --   - place 0 in row i
-- -- NOTE: this module defines the GCD as always non-negative, but we don't bother
-- --       constraining this method to produce the correct sign since explicit sign
-- --       correction ends up being required for the top row regardless
-- doGcd :: Int -> Int -> Int -> IVectors -> IVectors
-- doGcd pivot k i m = m // [ (i, newI), (k, newK) ]
--   where (newI, newK) = rowGcd pivot (m ! i, m ! k)

-- -- negate a row if necessary to make a pivot positive
-- doSignCorrection :: Int -> Int -> IVectors -> IVectors
-- doSignCorrection pivot k m = m // [ (k, newK) ]
--   where newK = reallyAssert ((m ! k ! pivot) /= 0)
--                $ fmap (* signum (m ! k ! pivot)) (m ! k)

-- -- looking at rows k and i and a given pivot column,
-- -- use row addition to:
-- --   - reduce row i modulo row k
-- doReduce :: Int -> Int -> Int -> IVectors -> IVectors
-- doReduce pivot k i m = m // [ (i, newI) ]
--   where newI = vmod pivot (m ! i) (m ! k)

-------------------------------------------------------------------------

-- withTranspose :: (IMatrix -> IMatrix) -> (IMatrix -> IMatrix)
-- withTranspose f = Matrix.transpose . f . Matrix.transpose


isStrictlyAscending :: (Ord a)=> [a] -> Bool
isStrictlyAscending xs = and $ zipWith (<=) xs (tail xs)

-- splitAtNothing :: [Maybe a] -> ([a], [Maybe a])
-- splitAtNothing = first (maybe (error "splitAtNothing: unpossible") id . sequence) . splitOn isNothing

-- splitOn :: (a -> Bool) -> [a] -> ([a],[a])
-- splitOn pred xs = let i = maybe (length xs) id $ findIndex pred xs
--                   in splitAt i xs



-- matrixToVectors :: IMatrix -> IVectors
-- matrixToVectors = Vector.fromList . fmap Vector.fromList . Matrix.toList
-- vectorsToMatrix :: IVectors -> IMatrix
-- vectorsToMatrix = Matrix.fromList . Vector.toList . fmap Vector.toList

isJustAnd :: (a -> Bool) -> Maybe a -> Bool
isJustAnd = maybe False

infix 0 ..<
(..<) :: (Enum b)=> b -> b -> [b]
a ..< b = [a..pred b]

------------------------------------------------------------------
-- BEGIN TEST CODE

data Size = Size !Int deriving (Show)
instance Arbitrary Size where
    -- FIXME (low priority) -- support dimensions of 0?
    arbitrary = Size <$> choose (1, 10)

data Sizes = Sizes !Size !Size deriving (Show)
instance Arbitrary Sizes where
    -- FIXME (low priority) -- support non-square
    arbitrary = arbitrary >>= \x -> pure (Sizes x x)

instance Arbitrary TriangularForm where arbitrary = QuickCheck.elements [Upper, Lower]

-- FIXME (low priority) -- support column-based
instance Arbitrary RowOrColumnCentric where arbitrary = pure RowCentric

data TestMatrix = TestMatrix !IMatrix deriving (Show)
instance Arbitrary TestMatrix where
    -- FIXME (low priority) -- support non-invertible
    arbitrary = TestMatrix <$> (arbMatrix `QuickCheck.suchThat` ((0 /=) . Impl.integralDeterminant))
      where
        arbMatrix = do
            Sizes (Size n) (Size m) <- arbitrary
            mat <- replicateM n . replicateM m
                    $ (*) <$> QuickCheck.elements [1, -1]
                          <*> choose (0,128)
            pure $ mat

    shrink (TestMatrix [ ]) = []
    shrink (TestMatrix [_]) = []
    shrink (TestMatrix m) = TestMatrix <$>
        filter ((/= 0) . Impl.integralDeterminant)
        [ lowerMinor m, upperMinor m ]
      where
        lowerMinor = tail . fmap tail
        upperMinor = init . fmap init

testSuite_0b3c9ae2_c8e6_4858_aa87_f6b0381afb6b :: TestTree
testSuite_0b3c9ae2_c8e6_4858_aa87_f6b0381afb6b =
  "Hnf tests" ~:
    [ "Unit tests for np-linear" ~: unitTestsForImpl
    , "Unit tests" ~: unitTests
    , "QC Product" ~: qc propProduct
    , "QC Reduced Entries" ~: qc propReducedEntries
    , "QC Pivot Locs" ~: qc propPivotLocs
    ]

-- Test the horribly underspecified output of np-linear's hermite routine
-- so that we can understand what it actually does.
unitTestsForImpl :: [TestTree]
unitTestsForImpl =
    -- LEGEND:       INPUT           HNF
    [ "identity" ~:
        check [ [    [ 1, 0],      [ 1, 0] ]
              , [    [ 0, 1],      [ 0, 1] ]
              ]

    , "no work" ~:
        check [ [ [ 4, 3, 0],   [ 4, 3, 0] ]
              , [ [ 0, 8, 2],   [ 0, 8, 2] ]
              , [ [ 0, 0, 7],   [ 0, 0, 7] ]
              ]

    , "needs sign corrections" ~:#
      [ check [ [    [ 5, 0],      [ 5, 0] ]
              , [    [ 0, 7],      [ 0, 7] ]
              ]
      , check [ [    [-5, 0],      [ 5, 0] ]
              , [    [ 0, 7],      [ 0, 7] ]
              ]
      , check [ [    [ 5, 0],      [ 5, 0] ]
              , [    [ 0,-7],      [ 0, 7] ]
              ]
      , check [ [    [-5, 0],      [ 5, 0] ]
              , [    [ 0,-7],      [ 0, 7] ]
              ]
      ]

    -- LEGEND:        INPUT           HNF
    , "1x1" ~:#
      [ check [ [       [ 1],         [ 1] ]
              ]
      , check [ [       [ 5],         [ 5] ]
              ]
      , check [ [       [-1],         [ 1] ]
              ]
      ]

    , "permutation" ~:
        check [ [ [ 0, 0, 1],   [ 1, 0, 0] ]
              , [ [ 1, 0, 0],   [ 0, 1, 0] ]
              , [ [ 0, 1, 0],   [ 0, 0, 1] ]
              ]

    -- LEGEND:        INPUT           HNF
    , "needs row addition" ~:#
      [ check [ [    [ 1, 2],      [ 1, 0] ]
              , [    [ 0, 1],      [ 0, 1] ]
              ]
      , check [ [    [ 1, 0],      [ 1, 0] ]
              , [    [ 2, 1],      [ 0, 1] ]
              ]
      ]

    , "degenerate" ~:#
      [ check [ [ [ 1, 2, 3],   [ 1, 2, 3] ]
              , [ [ 1, 2, 3],   [ 0, 0, 0] ]
              , [ [ 1, 2, 3],   [ 0, 0, 0] ]
              ]
      , check [ [ [ 3, 6, 9],   [ 1, 2, 3] ]
              , [ [ 2, 4, 6],   [ 0, 0, 0] ]
              , [ [ 1, 2, 3],   [ 0, 0, 0] ]
              ]
      ]

    , "nonsquare" ~:#
      [ check [ [ [ 3, 6, 9],   [ 1, 2, 3] ]
              , [ [ 2, 4, 6],   [ 0, 0, 0] ]
              ]
      , check [ [    [ 1, 2],      [ 1, 0] ]
              , [    [ 1, 1],      [ 0, 1] ]
              , [    [ 2, 1],      [ 0, 0] ]
              ]
      ]
    ]
  where
    check :: [[[Integer]]] -> Assertion
    check visualMatrices = expectHnf @=? actualHnf
      where
        [input, expectHnf] = transpose visualMatrices
        --(Impl.Hermite actualHnf _ _) = Impl.hermite input
        actualHnf = Impl.rowUpperHnfPart $ Impl.semiHermite' input

unitTests :: [TestTree]
unitTests =
    -- LEGEND:                       INPUT         UNIMODULAR     HNF
    --                                             (IF KNOWN)
    [ "identity" ~:
        check Lower RowCentric [ [    [ 1, 0],      [ 1, 0],    [ 1, 0] ]
                               , [    [ 0, 1],      [ 0, 1],    [ 0, 1] ]
                               ]

    , "no work" ~:
        check Lower RowCentric [ [ [ 4, 0, 0],   [ 1, 0, 0], [ 4, 0, 0] ]
                               , [ [ 3, 8, 0],   [ 0, 1, 0], [ 3, 8, 0] ]
                               , [ [ 0, 2, 7],   [ 0, 0, 1], [ 0, 2, 7] ]
                               ]

    , "needs sign corrections" ~:#
      [ check Lower RowCentric [ [    [ 5, 0],      [ 1, 0],    [ 5, 0] ]
                               , [    [ 0, 7],      [ 0, 1],    [ 0, 7] ]
                               ]
      , check Lower RowCentric [ [    [-5, 0],      [-1, 0],    [ 5, 0] ]
                               , [    [ 0, 7],      [ 0, 1],    [ 0, 7] ]
                               ]
      , check Lower RowCentric [ [    [ 5, 0],      [ 1, 0],    [ 5, 0] ]
                               , [    [ 0,-7],      [ 0,-1],    [ 0, 7] ]
                               ]
      , check Lower RowCentric [ [    [-5, 0],      [-1, 0],    [ 5, 0] ]
                               , [    [ 0,-7],      [ 0,-1],    [ 0, 7] ]
                               ]
      ]

    -- LEGEND:                       INPUT         UNIMODULAR     HNF
    --                                             (IF KNOWN)
    , "needs row addition" ~:#
      [ check Upper RowCentric [ [    [ 1, 2],      [ 1, 2],    [ 1, 0] ]
                               , [    [ 0, 1],      [ 0, 1],    [ 0, 1] ]
                               ]
      , check Upper RowCentric [ [    [ 1, 0],      [ 1, 0],    [ 1, 0] ]
                               , [    [ 2, 1],      [ 2, 1],    [ 0, 1] ]
                               ]
      ]

    , "1x1" ~:#
      [ check Lower RowCentric [ [       [ 1],         [ 1],       [ 1] ]
                               ]
      , check Lower RowCentric [ [       [ 5],         [ 1],       [ 5] ]
                               ]
      , check Lower RowCentric [ [       [-1],         [-1],       [ 1] ]
                               ]
      ]

    , "permutationL" ~:
        check Lower RowCentric [ [ [ 0, 0, 1],   [ 0, 0, 1], [ 1, 0, 0] ]
                               , [ [ 1, 0, 0],   [ 1, 0, 0], [ 0, 1, 0] ]
                               , [ [ 0, 1, 0],   [ 0, 1, 0], [ 0, 0, 1] ]
                               ]
    , "permutationU" ~:
        check Upper RowCentric [ [ [ 0, 0, 1],   [ 0, 0, 1], [ 1, 0, 0] ]
                               , [ [ 1, 0, 0],   [ 1, 0, 0], [ 0, 1, 0] ]
                               , [ [ 0, 1, 0],   [ 0, 1, 0], [ 0, 0, 1] ]
                               ]

    , "failed cases from QC" ~:#
      [ check Lower RowCentric [ [ [-3, 0, 0],           [], [ 3, 0, 0] ]
                               , [ [-5, 0, 1],           [], [ 1, 4, 0] ]
                               , [ [-1,-4,-6],           [], [ 1, 0, 1] ]
                               ]
      ]
    ]

  where
    check :: TriangularForm -> RowOrColumnCentric -> [[[Integer]]] -> Assertion
    -- (match on whether or not unimodular matrix was supplied)
    check form rc visualMatrices | null (visualMatrices !! 0 !! 1) = testHnf
                                 | otherwise                       = testBoth
      where
        [input, expectUni, expectHnf] = transpose visualMatrices
        (HnfDecomp actualHnf actualUni) = hnfDecomp form rc input
        testBoth = (expectUni, expectHnf) @=? (actualUni, actualHnf)
        testHnf  = expectHnf @=? actualHnf
    checkAll p = [ "upper row-based" ~: check Upper RowCentric p
                 , "lower row-based" ~: check Lower RowCentric p
                 , "upper col-based" ~: check Upper ColumnCentric p
                 , "lower col-based" ~: check Lower ColumnCentric p
                 ]


propProduct :: Property
propProduct =
    property $ \form rc (TestMatrix m) ->
        let (HnfDecomp hnf unimod) = hnfDecomp form rc m in
        case rc of RowCentric    -> unimod `Impl.matrixProduct` hnf === m
                   ColumnCentric -> hnf `Impl.matrixProduct` unimod === m

propReducedEntries :: Property
propReducedEntries =
    property $ \form rc (TestMatrix m) ->
        let m' = toHnf form rc m
        in all (checkColumn . flip matrixColumn m') (0 ..< length m')
  where
    --checkColumn xs | traceShow xs False = undefined
    checkColumn [] = error "propReducedEntries: zero column (unexpected for invertible M)"
    checkColumn (0:xs) = checkColumn xs
    checkColumn (x:xs) = all (liftM2 (&&) (0 <=) (< x)) xs

propPivotLocs :: Property
propPivotLocs =
    property $ \form rc (TestMatrix m) ->
        case form of Lower -> (lowerPivotLocsAreCorrect . toHnf form rc $ m)
                     Upper -> (upperPivotLocsAreCorrect . toHnf form rc $ m)

lowerPivotLocsAreCorrect :: IMatrix -> Bool
lowerPivotLocsAreCorrect =
    -- empty rows at top, followed by increasing pivots
    isJustAnd isStrictlyAscending . sequence
    . deleteNothingPrefix . fmap lowerPivotLoc

upperPivotLocsAreCorrect :: IMatrix -> Bool
upperPivotLocsAreCorrect =
    -- empty rows at bottom, followed by increasing pivots
    isJustAnd isStrictlyAscending . sequence
    . deleteNothingSuffix . fmap upperPivotLoc

----------------------------------------------------------------

deleteNothingPrefix :: [Maybe a] -> [Maybe a]
deleteNothingPrefix = dropWhile isJust
deleteNothingSuffix :: [Maybe a] -> [Maybe a]
deleteNothingSuffix = reverse . deleteNothingPrefix . reverse

lastMay :: [a] -> Maybe a
lastMay v | null v    = Nothing
          | otherwise = Just (last v)
lowerPivotLoc :: (Num a, Eq a)=> [a] -> Maybe Int
lowerPivotLoc = lastMay . findIndices (/= 0)
upperPivotLoc :: (Num a, Eq a)=> [a] -> Maybe Int
upperPivotLoc = findIndex (/= 0)

matrixColumn :: Int -> [[a]] -> [a]
matrixColumn i = fmap (!! i)

-- similarity transform under P, where P is the reverse permutation
matrixReverse :: [[a]] -> [[a]]
matrixReverse = reverse . fmap reverse
