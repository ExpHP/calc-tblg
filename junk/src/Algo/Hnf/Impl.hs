{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
Adaption of HNF-computing code from the package 'np-linear' (BSD3 licensed)
because everything I've written has bugs out the ass.

It is adapted to have all the necessary parts defined in one module, and, of course,
to not depend on NumericPrelude.
-}

module Algo.Hnf.Impl(
    Hermite(..),
    semiHermite,
    hermite',
    matrixProduct,
    integralDeterminant,
    ) where

import           "base" Data.Ratio
import           "base" Data.Function(on)
import           "base" Data.List hiding (transpose)
import           "base" Control.Arrow
import qualified "containers" Data.Map as Map

type MatrixF k = Matrix k -> Matrix k

-- divModUpperTriangular :: Vector Integer -> Matrix Integer -> (Vector Integer,Vector Integer)
-- divModUpperTriangular x u = reverse *** reverse $ divModLowerTriangular (reverse x) (reverse . map reverse $ u)

-- divModLowerTriangular :: Vector Integer -> Matrix Integer -> (Vector Integer,Vector Integer)
-- divModLowerTriangular x l = go l x [] [] where
--   go []        []        q r = (q,r)
--   go (lᵢ : l') (xᵢ : x') q r = go l' x' (q ++ [qᵢ]) (r ++ [rᵢ]) where
--     (lFirst,lᵢᵢ : _) = splitAt (length q) lᵢ
--     y = xᵢ - innerProduct q lFirst
--     (qᵢ,rᵢ)
--       | lᵢᵢ == 0  = error "Algebra.Linear.divModLowerTriangular: zero on diagonal"
--       | otherwise = y `divMod` lᵢᵢ

ratioToIntegralChecked :: (Integral a, Show a)=> Ratio a -> a
ratioToIntegralChecked r
    | denominator r /= 1 = error $ "ratioToIntegralChecked: " ++ show r
    | otherwise          = numerator r

-- | Decomposition of a matrix as A = U H, with U unimodular,
--   and H in upper triangular Hermite Normal Form
data Hermite = Hermite
    { rowUpperHnfPart :: Matrix Integer              -- ^ H per above definition
    , rowUpperUnimodularPart :: Matrix Integer       -- ^ U per above definition
    , rowUpperUnimodularTransform :: Matrix Integer  -- ^ Inverse of U (matrix such that U A = H)
    }

type Z = Integer

-- | Internal-use only API for describing operations represented by unimodular matrices
--   applied on the left of a matrix.
class UnimodularOps m where
    -- | Focus on the submatrix without the top row.
    recurseH :: (m -> m) -> (m -> m)
    -- | Focus on the submatrix without the left column.
    recurseV :: (m -> m) -> (m -> m)
    -- | Multiply a row by a root of unity.
    rowMul :: Int -> Z -> m -> m
    -- | Switch two rows.
    rowSwap :: Int -> Int -> m -> m
    -- | Add a multiple of one row into a different row.
    rowAdd :: Int -> Int -> Z -> m -> m

    default recurseH :: (Newtype o m, UnimodularOps o)=> (m -> m) -> (m -> m)
    default recurseV :: (Newtype o m, UnimodularOps o)=> (m -> m) -> (m -> m)
    default rowMul   :: (Newtype o m, UnimodularOps o)=> Int -> Z -> m -> m
    default rowSwap  :: (Newtype o m, UnimodularOps o)=> Int -> Int -> m -> m
    default rowAdd   :: (Newtype o m, UnimodularOps o)=> Int -> Int -> Z -> m -> m
    recurseH = wrap1 . recurseH . unwrap1
    recurseV = wrap1 . recurseV . unwrap1
    rowMul a b = wrap1 (rowMul a b)
    rowSwap a b = wrap1 (rowSwap a b)
    rowAdd a b c = wrap1 (rowAdd a b c)

class Transpose t where
    transpose :: t -> t

    default transpose :: (Newtype o t, UnimodularOps o)=> t -> t
    transpose = wrap1 transpose

{-# RULES
"transpose/transpose"    transpose . transpose = id
#-}

------------------------------------------------------------------------

class Newtype o n | n -> o where
    wrap    :: o -> n
    unwrap  :: n -> o

    wrap1   :: (o -> o) -> (n -> n)
    unwrap1 :: (n -> n) -> (o -> o)
    wrap1   f = wrap . f . unwrap
    unwrap1 f = unwrap . f . wrap

newtype Paranoid a = Paranoid { unParanoid :: a }
                     deriving (Eq, Show, Transpose)
newtype Transposing a = Transposing { unTransposing :: a }
                        deriving (Eq, Show)
newtype Subtracting a = Subtracting { unSubtracting :: a }
                        deriving (Eq, Show, Transpose)

instance Newtype a (Paranoid a) where wrap = Paranoid
                                      unwrap = unParanoid
instance Newtype a (Transposing a) where wrap = Transposing
                                         unwrap = unTransposing
instance Newtype a (Subtracting a) where wrap = Subtracting
                                         unwrap = unSubtracting

instance (UnimodularOps m)=> UnimodularOps (Paranoid m) where
    rowMul k (-1) = wrap1 (rowMul k (-1))
    rowMul k ( 1) = wrap1 (rowMul k ( 1))
    rowMul k c    = error $ "rowMul: not a root of unity " ++ parens (show (k,c))

    rowAdd i j c | i == j    = error $ "rowAdd: src == dest " ++ parens (show (i,j,c))
                 | otherwise = wrap1 (rowAdd i j c)

instance (UnimodularOps m, Transpose m)=> UnimodularOps (Transposing m) where
    recurseH = wrap1 . recurseV . unwrap1
    recurseV = wrap1 . recurseH . unwrap1

    rowMul  i   c = wrap1 (transpose . rowMul  i   c . transpose)
    rowSwap i j   = wrap1 (transpose . rowSwap i j   . transpose)
    rowAdd  i j c = wrap1 (transpose . rowAdd  i j c . transpose)

instance (UnimodularOps m)=> UnimodularOps (Subtracting m) where
    rowAdd i j c = wrap1 (rowAdd i j (-c))

------------------------------------------------------------------------

parens :: String -> String
parens s = "(" ++ s ++ ")"



-- semiHermite :: Matrix Integer -> Hermite
-- semiHermite fullM =
--     let (h, u) = go fullM
--     in Hermite h (unimodularInv u) u
--     -- HACK the inverse of u can be obtained more elegantly by simply negating all the
--     --       column-addition operations involved in the creation of u
--   where
--     unimodularInv = fmap (fmap ratioToIntegralChecked)
--                   . maybe (error "hermite: bug! (inverse of unimodular failed)") id
--                   . invert
--                   . fmap (fmap fromIntegral)
--     go []          = ([],[])
--     go xs@([] : _) = (xs, identity (length xs))
--     go vs          = case minimumAmong (compare `on` (abs . head . fst)) nonZero of
--       Nothing                    -> first (map (0 :)) $ go (map tail vs)
--       Just ((v,i),[])            -> case go (map (tail . fst) startZero) of
--         (h,u) -> ((positive v :) . map (0 :) $ h,changeSign v `matrixProduct` rowSwap n (1,i) `matrixProduct` shift u)
--       Just ((v@(v₀ : _),i),rest) -> (h, u')
--         where
--           (reduced,translates) =
--             unzip . flip map rest $ \ (x@(x₀ : _),j) ->
--               let (c,_) = x₀ `divMod` v₀
--               in ((zipWith (-) x (fmap (c *) v), j), (j,c))
--           (h,u) = go $ v : map fst reduced ++ map fst startZero
--           -- wait a second, is this... actually performing dense matrix products just to swap rows !?
--           u' = u `matrixProduct` permute (i : map snd reduced ++ map snd startZero)
--                  `matrixProduct` affine n i (map (second negate) translates)
--      where
--       (startZero,nonZero) = partition ((==) 0 . head . fst) . flip zip [1 ..] $ vs
--       positive v = map (* signum (head v)) v
--       changeSign v = matrixFromFunction n n f where
--         f i j
--           | i == j && i == 1 = signum (head v)
--           | i == j           = 1
--           | otherwise        = 0
--       shift u = (1 : replicate (n - 1) 0) : map (0 :) u
--       n = length vs


-- adjoint :: Matrix Integer -> Matrix Integer
-- adjoint m = strip . fst . hermite . adorn $ m where
--   n = length m
--   strip = map (drop n)

-- transform an integer matrix into "Hermite Semi-Normal Form", which is a name
--   I made up to describe the form returned by np-linear's 'hermite' function.
--
-- It is Hermite Normal Form, minus the constraint that non-pivots be reduced
--  modulo their respective pivots. (it is "merely" triangular)
hermite' :: Matrix Integer -> Matrix Integer
hermite' []          = []
hermite' xs@([] : _) = xs
hermite' vs          = case minimumAmong (compare `on` (abs . head)) nonZero of
  Nothing                -> map (0 :) $ hermite' (map tail vs)
  Just (v,[])            -> (positive v :) . map (0 :) $ hermite' (map tail startZero)
  Just (v@(v₀ : _),rest) -> hermite' $ v : reduced ++ startZero where
    reduced = map (\ x@(x₀ : _) -> let (c,_) = x₀ `divMod` v₀ in zipWith (-) x (fmap (c *) v)) rest
 where
  (startZero,nonZero) = partition ((==) 0 . head) vs
  positive v = map (* signum (head v)) v

type Vector k
  = [k]

type Matrix k
  = [Vector k]

-- -- A relation among vectors of degree 'd' over field 'k'
-- data Relation k
--   = Relation { getRelation :: [k] }
--   deriving (Show)

-- satisfies :: (Num k,Eq k) => Vector k -> Relation k -> Bool
-- satisfies v (Relation r) = innerProduct v r == 0

-- -- | Calculate all dependencies among the given vectors of degree d.
-- dependencies :: (Fractional k,Eq k) => Integer -> [Vector k] -> [Relation k]
-- dependencies d = map (Relation . genericDrop d) . filter (all (== zero) . genericTake d) . (\ (r,_,_) -> r) . reduce . adorn

-- -- | Calculate the equations satisfied by the subspace spanned by the given vectors of degree d.
-- equations :: (Fractional k,Eq k) => Integer -> [Vector k] -> [Relation k]
-- equations d vs = dependencies (genericLength vs + 1) . transpose . (:) (genericReplicate d zero) $ vs

-- -- | Solve the given equations, in the form of a basis of vectors of degree d.
-- solve :: (Fractional k,Eq k) => Integer -> [Relation k] -> [Vector k]
-- solve d = map getRelation . equations d . map getRelation

-- inverseImage :: (Fractional k,Eq k) => Matrix k -> Vector k -> Vector k
-- inverseImage a = solveUpperTriangular u . matrixVector b where
--   (u,b,_) = reduce a

invert :: (Fractional k,Eq k) => Matrix k -> Maybe (Matrix k)
invert m = fmap strip . process . (\ (r,_,_) -> r) . reduce . adorn $ m where
  n = length m
  process x = go x where
    go [v] = Just [v]
    go (r@(pivot : r') : rest)
      | pivot == 1 = do
        m' <- go (map tail rest)
        rowsToAdd <- sequence . zipWith3 f [0 ..] r' $ m'
        return $ (1 : foldr (zipWith (+)) r' rowsToAdd) : map (0 :) m'
      | otherwise = Nothing
    f i c v
      | v₀ == 0   = Nothing
      | otherwise = Just $ map ((*) (negate c / v₀)) v
      where
        v₀ = v !! i
  strip = map (drop n)

determinant :: (Fractional k,Eq k) => Matrix k -> k
determinant x = case reduce x of (_,_,σ) -> σ

-- adjoint :: (Fractional k,Eq k) => Matrix k -> Maybe (Matrix k)
-- adjoint a = fmap (fmap (abs (determinant a) *)) <$> invert a

-- diagonal :: Matrix k -> [k]
-- diagonal []               = []
-- diagonal ((d : _) : rest) = d : diagonal (map tail rest)

matrixProduct :: (Num k) => Matrix k -> Matrix k -> Matrix k
matrixProduct a b = map (($ transpose b) . map . innerProduct) a

-- matrixVector :: (Num k) => Matrix k -> Vector k -> Vector k
-- matrixVector a = map (\ [x] -> x) . matrixProduct a . map (: [])

innerProduct :: (Num k) => Vector k -> Vector k -> k
innerProduct a b = sum (zipWith (*) a b)

matrixFromFunction :: Int -> Int -> (Int -> Int -> k) -> Matrix k
matrixFromFunction m n f = [[f i j | j <- [1 .. n]] | i <- [1 .. m]]

identity :: (Num k) => Int -> Matrix k
identity n = matrixFromFunction n n δ


-- Triangular decomposition

-- solveUpperTriangular :: (Fractional k,Eq k) => Matrix k -> Vector k -> Vector k
-- solveUpperTriangular u x = reverse $ solveLowerTriangular (reverse . map reverse $ u) (reverse x)

-- solveLowerTriangular :: (Fractional k,Eq k) => Matrix k -> Vector k -> Vector k
-- solveLowerTriangular l x = go l x [] where
--   go []        []        q = q
--   go (lᵢ : l') (xᵢ : x') q = go l' x' (q ++ [qᵢ]) where
--     (lFirst,lᵢᵢ : _) = splitAt (length q) lᵢ
--     y = xᵢ - innerProduct q lFirst
--     qᵢ
--       | lᵢᵢ == 0  = error "Linear.solveLowerTriangular: zero on diagonal"
--       | otherwise = y / lᵢᵢ

-- Compute the row echelon form of the matrix,
-- together with the basis transformation matrix,
-- and its determinant.
reduce :: (Fractional k,Eq k) => Matrix k -> (Matrix k,Matrix k,k)
reduce []          = ([],[],1)
reduce xs@([] : _) = (xs,identity (length xs),1)
reduce vs          = case nonZero of
  []                    -> (\ (x,u,_σ) -> (map (0 :) x,u,0)) $ reduce (map tail vs)
  (v@(v₀ : _),i) : []   -> let
    subMatrix = map (tail . fst) startZero
    (h,u,σ) = reduce subMatrix
    sign = if odd i then 1 else -1
   in
    ( (map (/ v₀) v :) . map (0 :) $ h
    , normalisation v₀ `matrixProduct` rowSwap n (1,i) `matrixProduct` shift u
    , sign * v₀ * σ
    )
  (v@(v₀ : _),i) : rest -> let
    (reduced,translates) = unzip . flip map rest $ \ (x@(x₀ : _),j) -> let
      c = x₀ / v₀ in ((zipWith (\ vᵢ xᵢ -> xᵢ - c * vᵢ) v x,j),(j,c))
    subMatrix = v : map fst reduced ++ map fst startZero
    (h,u,σ) = reduce subMatrix
    permutation = i : map snd reduced ++ map snd startZero
   in
    ( h
    , u
        `matrixProduct` permute permutation
        `matrixProduct` affine n i (map (second negate) translates)
    , permutationSign permutation * σ
    )
 where
  (startZero,nonZero) = partition ((==) 0 . head . fst) . flip zip [1 ..] $ vs
  normalisation v₀ = matrixFromFunction n n f where
    f 1 1 = 1 / v₀
    f i j = δ i j
  shift u = (1 : replicate (n - 1) 0) : map (0 :) u
  n = length vs

-- rowSwap :: (Num k) => Int -> (Int,Int) -> Matrix k
-- rowSwap n (k,l) = matrixFromFunction n n f where
--   f i j
--     | i == k && j == l           = one
--     | i == l && j == k           = one
--     | i == j && i /= k && i /= l = one
--     | otherwise                  = zero

rowSwapF :: (Int,Int) -> MatrixF k
rowSwapF = error "TODO: swap rows"

-- this is... the identity matrix plus... um...
--  a column at index k, given in a sparse format?
affine :: (Num k) => Int -> Int -> [(Int,k)] -> Matrix k
affine n k ps = matrixFromFunction n n $ \ i j -> δ i j + c i j where
  m = Map.fromList ps
  c i j = case (Map.lookup i m,j == k) of
    (Just cᵢ,True) -> cᵢ
    _              -> zero

-- Permutes rows, I think.
permute :: (Num k) => [Int] -> Matrix k
permute is = matrixFromFunction n n (\ i j -> δ (is !! (i - 1)) j) where
  n = length is

permuteF :: (Num k)=> [Int] -> MatrixF k
permuteF = error "TODO: permute rows"

permutationSign :: (Num k) => [Int] -> k
permutationSign [] = one
permutationSign (x : xs) = ε * permutationSign xs where
  inversions = length $ filter (x >) xs
  ε
    | even inversions = one
    | otherwise       = negate one

adorn :: (Num k,Eq k) => [[k]] -> [[k]]
adorn vs = map f . zip [1 .. n] $ vs where
  f (i,v) = v ++ map (δ i) [1 .. n]
  n = length vs

δ :: (Eq a,Num k) => a -> a -> k
δ i j
  | i == j    = one
  | otherwise = zero

-- findAmong :: (a -> Bool) -> [a] -> Maybe (a,[a])
-- findAmong p [] = Nothing
-- findAmong p (x : xs)
--   | p x       = Just (x,xs)
--   | otherwise = second (x :) <$> findAmong p xs

minimumAmong :: (a -> a -> Ordering) -> [a] -> Maybe (a,[a])
minimumAmong _ []  = Nothing
minimumAmong _ [x] = Just (x,[])
minimumAmong (~~) (x : y : xs) = fmap (second (l :)) $ minimumAmong (~~) (s : xs) where
  (s,l)
    | x ~~ y == LT = (x,y)
    | otherwise    = (y,x)

-- headE _ (x : _) = x
-- headE s _       = error s

one :: (Num a) => a
one = 1
zero :: (Num a) => a
zero = 0

--------------------------------
-- junk for working with list-of-list matrices, exported for Algo.Hnf's sake

integralDeterminant :: (Integral a)=> Matrix a -> a
integralDeterminant = fromIntegral . ratioToIntegralChecked . determinant . fmap (fmap toRational)
