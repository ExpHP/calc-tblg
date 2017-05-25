{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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

-- this is for Recurse
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
Adaption of HNF-computing code from the package 'np-linear' (BSD3 licensed)
because everything I've written has bugs out the ass.

It is adapted to have all the necessary parts defined in one module, and, of course,
to not depend on NumericPrelude.
-}

module Algo.Hnf.Impl2(
    UnimodularState,
    rowUpperHnfPart,
    rowUpperUnimodularTransform,
    rowUpperUnimodularPart,
--    semiHermite,
    hermite',
    semiHermite',
    semiHermite'',
    matrixProduct,
    integralDeterminant,
    ) where

import           "base" Data.Ratio
import           "base" Data.Function(on)
import           "base" Data.List
import           "base" Control.Arrow
import qualified "containers" Data.Map as Map

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
type UnimodularState = ([[Z]], [[Z]], [[Z]])
rowUpperHnfPart             :: UnimodularState -> [[Z]] -- ^ H per above definition
rowUpperHnfPart                (a,_,_)  = a
rowUpperUnimodularTransform :: UnimodularState -> [[Z]] -- ^ Q = Inverse of U (matrix such that Q A = H)
rowUpperUnimodularTransform    (_,q,_)  = q
rowUpperUnimodularPart      :: UnimodularState -> [[Z]] -- ^ U per above definition
rowUpperUnimodularPart         (_,_,u)  = u

trimap f g h (a, b, c) = (f a, g b, h c)

type Z = Integer

---------------------------------------------
-- primitive vector operations

-- | Get an integer multiple of a source row (second operand) which,
--   when added to a target row (first operand), reduces the element
--   in a given column such that 'abs remainder < abs dividend'
vDiv column = div `on` (vItem column)
vItem = flip (!!)

---------------------------------------------
-- primitive matrix operations

mRowMul  c   t xs = updateElem ((c *) <$>) t xs
mRowSwap   s t xs = updateElem (const (xs !! t)) s
                  . updateElem (const (xs !! s)) t
                  $ xs
mRowAdd  c s t xs = updateElem (zipWith (+) (fmap (c *) (xs !! s))) t xs
mRowUncons = maybe (error "unconsRow: empty") id . uncons
mRowCons   = uncurry (:)

mColMul  c   t = transpose . mRowMul  c   t . transpose
mColSwap   s t = transpose . mRowSwap   s t . transpose
mColAdd  c s t = transpose . mRowAdd  c s t . transpose
mColUncons = foldMap ((:[]) *** (:[]))
           . maybe (error "unconsCol: empty sublist") id
           . sequence . fmap uncons
mColCons   = uncurry $ zipWith (:)

---------------------------------------------
-- operations performed during unimodular reduction

-- A transforms like A.
uRowMulA  = mRowMul
uRowSwapA = mRowSwap
uRowAddA  = mRowAdd
uRowUnconsA = mRowUncons
uRowConsA   = mRowCons
uColUnconsA = mColUncons
uColConsA   = mColCons

-- Q transforms like A, by definition.
uRowMulQ  = mRowMul
uRowSwapQ = mRowSwap
uRowAddQ  = mRowAdd
-- however, it must remain square.  When A loses a row, Q loses both a row and a column
uRowUnconsQ = (\(r,(c,m)) -> ((r,c),m)) . second mColUncons . mRowUncons
uRowConsQ   = mRowCons . second mColCons . (\((r,c),m) -> (r,(c,m)))
uColUnconsQ = (,) ()
uColConsQ   = snd

-- When we transform U A = U' A' = U (inv Q) Q A,
-- we see that operations applied on the left of A have their
-- inverses applied on the right of U.
uRowMulU  c   t = mColMul    c    t
uRowSwapU   s t = mColSwap      s t
uRowAddU  c s t = mColAdd  (-c) s t
-- induction on U follows like induction on Q
uRowUnconsU = uRowUnconsQ
uRowConsU   = uRowConsQ
uColUnconsU = uColUnconsQ
uColConsU   = uColConsQ

-- Operations on the product (A, Q, U)
uRowMul  c   t = trimap (uRowMulA  c   t) (uRowMulQ  c   t) (uRowMulU  c   t)
uRowSwap   s t = trimap (uRowSwapA   s t) (uRowSwapQ   s t) (uRowSwapU   s t)
uRowAdd  c s t = trimap (uRowAddA  c s t) (uRowAddQ  c s t) (uRowAddU  c s t)
uRowUncons = convolute32 . trimap uRowUnconsA uRowUnconsQ uRowUnconsU
uRowCons   =               trimap uRowConsA   uRowConsQ   uRowConsU   . convolute23
uColUncons = convolute32 . trimap uColUnconsA uColUnconsQ uColUnconsU
uColCons   =               trimap uColConsA   uColConsQ   uColConsU   . convolute23

convolute32 ((a1,a2),(b1,b2),(c1,c2)) = ((a1,b1,c1),(a2,b2,c2))
convolute23 ((a1,b1,c1),(a2,b2,c2)) = ((a1,a2),(b1,b2),(c1,c2))

-- focus on a submatrix, minus the first row/column
uRowRecurse f = uRowCons . second f . uRowUncons
uColRecurse f = uColCons . second f . uColUncons

uRows (a,_,_) = a
uIsTrivial (a,_,_) = null a || null (head a)

------------------------------------------------------------------------

data PivotSearchResult a
    = WellThereArentAny
    | ThatWasTheOnlyOne a
    | OthersStillRemain a
    deriving (Functor)

minimumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMay _ [] = Nothing
minimumByMay f xs = Just $ minimumBy f xs

------------------------------------------------------------------------

-- | Extract the row with the smallest leading nonzero.
--   Nothing if all rows have a leading 0. (and there is at least one)
bestPivotRowToTop m = fmap (\i -> uRowSwap 0 i m)
                    . maybe WellThereArentAny
                            (case length nonzeros of 1 -> ThatWasTheOnlyOne
                                                     _ -> OthersStillRemain
                            )
                    . fmap fst
                    . minimumByMay (compare `on` (abs . vItem 0 . snd))
                    $ nonzeros
  where
    nonzeros = filter ((0 /=) . vItem 0 . snd) . zip [0..] . uRows $ m


updateElem :: (a -> a) -> Int -> [a] -> [a]
updateElem a b c = q a b c
  where
    q _ k _ | k < 0 = error "updateElem: n < 0"
    q f 0 (x:xs) = f x : xs
    q _ _ []     = error "updateElem: out of range"
    q f k (x:xs) = x : q f (pred k) xs

semiHermite' :: [[Z]] -> ([[Z]],[[Z]],[[Z]])
semiHermite' vs = semiHermite'' (vs, identity (length vs), identity (length vs))

semiHermite'' :: ([[Z]], [[Z]], [[Z]]) -> ([[Z]],[[Z]],[[Z]])
semiHermite'' vs | uIsTrivial vs = vs
semiHermite'' vs =
    case bestPivotRowToTop vs of
        WellThereArentAny     -> -- so let's stop looking at this column
                                 uColRecurse semiHermite'' vs
        ThatWasTheOnlyOne vs' -> -- so we'll fix the new pivot's sign and continue southeast
                                 uColRecurse (uRowRecurse semiHermite'')
                                 . uRowMul (signum . vItem 0 . head . uRows $ vs') 0
                                 $ vs'
        OthersStillRemain vs' -> -- so let's continue taking moduli until only one remains
                                 semiHermite'' reduced
          where
            (pivotRow : otherRows) = uRows vs'
            multiples = fmap (flip (vDiv 0) pivotRow) otherRows
            reduced = f vs'
              where f = foldr (.) id $ zipWith (\t c -> uRowAdd (-c) 0 t) [1..] multiples

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

-- invert :: (Fractional k,Eq k) => Matrix k -> Maybe (Matrix k)
-- invert m = fmap strip . process . (\ (r,_,_) -> r) . reduce . adorn $ m where
--   n = length m
--   process x = go x where
--     go [v] = Just [v]
--     go (r@(pivot : r') : rest)
--       | pivot == 1 = do
--         m' <- go (map tail rest)
--         rowsToAdd <- sequence . zipWith3 f [0 ..] r' $ m'
--         return $ (1 : foldr (zipWith (+)) r' rowsToAdd) : map (0 :) m'
--       | otherwise = Nothing
--     f i c v
--       | v₀ == 0   = Nothing
--       | otherwise = Just $ map ((*) (negate c / v₀)) v
--       where
--         v₀ = v !! i
--   strip = map (drop n)

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
    , normalisation v₀ `matrixProduct` rowSwapMat n (1,i) `matrixProduct` shift u
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

rowSwapMat :: (Num k) => Int -> (Int,Int) -> Matrix k
rowSwapMat n (k,l) = matrixFromFunction n n f where
  f i j
    | i == k && j == l           = one
    | i == l && j == k           = one
    | i == j && i /= k && i /= l = one
    | otherwise                  = zero

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

permutationSign :: (Num k) => [Int] -> k
permutationSign [] = one
permutationSign (x : xs) = ε * permutationSign xs where
  inversions = length $ filter (x >) xs
  ε
    | even inversions = one
    | otherwise       = negate one

-- adorn :: (Num k,Eq k) => [[k]] -> [[k]]
-- adorn vs = map f . zip [1 .. n] $ vs where
--   f (i,v) = v ++ map (δ i) [1 .. n]
--   n = length vs

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
