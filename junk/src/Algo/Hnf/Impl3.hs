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
    Hermite(..),
--    semiHermite,
    hermite',
    semiHermite'',
    matrixProduct,
    integralDeterminant,
    ) where

import Debug.Trace
import           "base" Data.Ratio
import           "base" Data.Function(on)
import           "base" Data.List hiding (transpose)
import qualified "base" Data.List as List
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
data Hermite = Hermite
    { rowUpperHnfPart :: Matrix Integer              -- ^ H per above definition
    , rowUpperUnimodularPart :: Matrix Integer       -- ^ U per above definition
    , rowUpperUnimodularTransform :: Matrix Integer  -- ^ Inverse of U (matrix such that U A = H)
    }

type Z = Integer

-- | Internal-use only API for describing operations represented by unimodular matrices
--   applied on the left of a matrix.
--
--   Only here to help break the problem down, nothing more.
class UnimodularOps m where
    -- | Multiply a row by a root of unity.
    rowMul :: Z -> Int -> m -> m
    -- | Switch two rows.
    rowSwap :: Int -> Int -> m -> m
    -- | Add a multiple of one row into a different row.
    rowAdd :: Z -> Int -> Int -> m -> m

    default rowMul    :: (Newtype o m, UnimodularOps o)=> Z -> Int -> m -> m
    default rowSwap   :: (Newtype o m, UnimodularOps o)=>      Int -> Int -> m -> m
    default rowAdd    :: (Newtype o m, UnimodularOps o)=> Z -> Int -> Int -> m -> m
    rowMul a b = wrap1 (rowMul a b)
    rowSwap a b = wrap1 (rowSwap a b)
    rowAdd a b c = wrap1 (rowAdd a b c)

class Transpose t where
    transpose :: t -> t

    default transpose :: (Newtype o t, Transpose o)=> t -> t
    transpose = wrap1 transpose

instance Transpose [[a]] where
    transpose = List.transpose

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

-- Transforms like the U in U A = U' A', rather than the A.
newtype UnimodularPart a = UnimodularPart { unUnimodularPart :: a }
                           deriving (Eq, Show)

instance Newtype a (Paranoid a) where wrap = Paranoid
                                      unwrap = unParanoid
instance Newtype a (UnimodularPart a) where wrap = UnimodularPart
                                            unwrap = unUnimodularPart

instance (UnimodularOps m)=> UnimodularOps (Paranoid m) where
    rowMul (-1) t = wrap1 (rowMul (-1) t)
    rowMul ( 1) t = wrap1 (rowMul ( 1) t)
    rowMul c    t = error $ "rowMul: not a root of unity " ++ parens (show (c,t))

    rowAdd c s t | s == t    = error $ "rowAdd: src == dest " ++ parens (show (c,s,t))
                 | otherwise = wrap1 (rowAdd c s t)

instance (UnimodularOps m, Transpose m)=> UnimodularOps (UnimodularPart m) where
    -- row ops act on the transpose
    rowMul  c   t = wrap1 (transpose . rowMul    c    t . transpose)
    rowSwap   s t = wrap1 (transpose . rowSwap      s t . transpose)
    rowAdd  c s t = wrap1 (transpose . rowAdd  (-c) s t . transpose)

instance (Recurse m r c)=> Recurse (UnimodularPart m) (r,c) () where
    isTrivial = isTrivial . unwrap
    unconsRow (UnimodularPart m) = ((r,c), UnimodularPart m'')
      where (r, m')  = unconsRow m
            (c, m'') = unconsCol m'
    consRow ((r,c), UnimodularPart m) = UnimodularPart $ consRow (r, consCol (c, m))
    unconsCol = (,) ()
    consCol   = snd

------------------------------------------------------------------------

data PivotSearchResult a
    = WellThereArentAny
    | ThatWasTheOnlyOne a
    | OthersStillRemain a
    deriving (Functor)

-- | More private implementation details.
class HasRows m row | m -> row where
    toRows :: m -> [row]

class Recurse m r c | m -> r c where
    -- | Focus on the submatrix without the top row.
    recurseDown  :: (m -> m) -> (m -> m)
    recurseDown  f = consRow . second f . unconsRow
    -- | Focus on the submatrix without the left column.
    recurseRight :: (m -> m) -> (m -> m)
    recurseRight f = consCol . second f . unconsCol
    -- | Is the matrix empty?  (0xn, nx0, or 0x0)
    isTrivial :: m -> Bool

    unconsRow :: m -> (r, m)
    unconsCol :: m -> (c, m)
    consRow   :: (r, m) -> m
    consCol   :: (c, m) -> m

-- NOTE: This instance is undecidable for some reason, but the typeclass is private
--       and we're ultimately only going to use one or two specific monomorphisations of it.
instance (Recurse a ar ac, Recurse b br bc)=> Recurse (a,b) (ar,br) (ac,bc) where
    isTrivial = isTrivial . fst
    unconsRow = convolute . (unconsRow *** unconsRow)
    unconsCol = convolute . (unconsCol *** unconsCol)
    consRow = (consRow *** consRow) . convolute
    consCol = (consCol *** consCol) . convolute
convolute :: ((a1,a2),(b1,b2)) -> ((a1,b1),(a2,b2))
convolute ((a1,a2),(b1,b2)) = ((a1,b1),(a2,b2))

instance HasRows [[Integer]] [Integer] where
    toRows = id

instance Recurse [[Integer]] [Integer] [Integer] where
    isTrivial = (||) <$> null <*> null . head
    unconsRow = maybe (error "unconsRow: empty") id . uncons
    consRow   = uncurry (:)
    unconsCol = foldMap (pure *** pure)
              . maybe (error "unconsCol: empty sublist") id
              . sequence . fmap uncons
    consCol   = uncurry $ zipWith (:)

instance RowOps [Integer] where
    rowItem = flip (!!)

minimumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumByMay _ [] = Nothing
minimumByMay f xs = Just $ minimumBy f xs

class RowOps row where
    -- | Get an integer multiple of a source row (second operand) which,
    --   when added to a target row (first operand), reduces the element
    --   in a given column such that 'abs remainder < abs dividend'
    rowItem :: Int -> row -> Z
    rowDiv :: Int -> row -> row -> Z
    rowDiv k = div `on` (rowItem k)

class LeftIdentity m where
    leftIdentity :: m -> m

instance LeftIdentity [[Integer]] where
    leftIdentity = identity . length

------------------------------------------------------------------------

parens :: String -> String
parens s = "(" ++ s ++ ")"


instance UnimodularOps [[Integer]] where
    rowMul  c   t    = updateElem ((c *) <$>) t
    rowSwap   s t xs = updateElem (const (xs !! t)) s
                     . updateElem (const (xs !! s)) t
                     $ xs
    rowAdd  c s t xs = updateElem (zipWith (+) (fmap (c *) (xs !! s))) t xs

-- | Extract the row with the smallest leading nonzero.
--   Nothing if all rows have a leading 0. (and there is at least one)
bestPivotRowToTop :: (UnimodularOps m, HasRows m row, RowOps row)=> m -> PivotSearchResult m
bestPivotRowToTop m = fmap (\i -> rowSwap 0 i m)
                    . maybe WellThereArentAny
                            (case length nonzeros of 1 -> ThatWasTheOnlyOne
                                                     _ -> OthersStillRemain
                            )
                    . fmap fst
                    . minimumByMay (compare `on` (abs . rowItem 0 . snd))
                    $ nonzeros
  where
    nonzeros = filter ((0 /=) . rowItem 0 . snd) . zip [0..] . toRows $ m


instance (UnimodularOps a, UnimodularOps b)=> UnimodularOps (a,b) where
    rowMul  c   t = rowMul  c   t *** rowMul  c   t
    rowSwap   s t = rowSwap   s t *** rowSwap   s t
    rowAdd  c s t = rowAdd  c s t *** rowAdd  c s t

data UnimodularTrans m = UnimodularTrans
    { transTheMatrix     :: m
    , transUnimodular    :: m
    , transUnimodularInv :: UnimodularPart m
    }

umtToProduct   (UnimodularTrans a b c) = (a,(b,c))
umtFromProduct (a,(b,c)) = UnimodularTrans a b c

instance (UnimodularOps m, Transpose m)=> UnimodularOps (UnimodularTrans m) where
    rowMul  c   t = umtFromProduct . rowMul  c   t . umtToProduct
    rowSwap   s t = umtFromProduct . rowSwap   s t . umtToProduct
    rowAdd  c s t = umtFromProduct . rowAdd  c s t . umtToProduct

instance (HasRows m row)=> HasRows (UnimodularTrans m) row where
    toRows = toRows . transTheMatrix

instance (Recurse m r c)=> Recurse (UnimodularOps (UnimodularTrans m)) (r,((r,c),(r,c))) (c,((),())) where
    unconsRow = fmap umtFromProduct . unconsRow . umtToProduct

updateElem :: (a -> a) -> Int -> [a] -> [a]
updateElem a b c = trace "enter updateElem" $ q a b c
  where
    q _ k xs | traceShow (k, length xs) False = undefined
    q _ k _ | k < 0 = error "updateElem: n < 0"
    q f 0 (x:xs) = f x : xs
    q _ _ []     = error "updateElem: out of range"
    q f k (x:xs) = x : q f (pred k) xs

semiHermite' :: (LeftIdentity t, Transpose t, UnimodularOps t, HasRows t row, Recurse t a b)=> t -> UnimodularTrans t
semiHermite' m = semiHermite'' (UnimodularTrans m (leftIdentity m) (wrap $ leftIdentity m))

semiHermite'' :: (UnimodularOps t, HasRows t row, Recurse t a b)=> t -> t
semiHermite'' vs | isTrivial vs = vs
semiHermite'' vs =
    case bestPivotRowToTop vs of
        WellThereArentAny     -> -- so let's stop looking at this column
                                 recurseRight semiHermite'' vs
        ThatWasTheOnlyOne vs' -> -- so we'll fix the new pivot's sign and continue southeast
                                 recurseRight (recurseDown semiHermite'')
                                 . rowMul (signum . rowItem 0 . head . toRows $ vs') 0
                                 $ vs'
        OthersStillRemain vs' -> -- so let's continue taking moduli until only one remains
                                 semiHermite'' reduced
          where
            (pivotRow:otherRows) = toRows vs'
            multiples = fmap (flip (rowDiv 0) pivotRow) otherRows
            reduced = f vs'
              where f = foldr (.) id $ zipWith (\t c -> rowAdd (-c) 0 t) [1..] multiples

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
