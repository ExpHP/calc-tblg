{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import My.Lonely.CRT(chineseRemainder)
import qualified Number.SqrtRatio as SqrtRatio
import Math.Integers.Try
import qualified Data.Aeson as Aeson

import GHC.Generics
import Control.Applicative
import Control.Monad
import Data.Function
import Number.SqrtRatio(SqrtRatio(..), isSquarefree)
import Debug.Trace
import Data.Ratio
import qualified Data.Set as Set
import Math.NumberTheory.ArithmeticFunctions(divisors)
import Data.Foldable
import GHC.Real(Ratio(..))
import Math.NumberTheory.Moduli(invertMod)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Environment(getArgs)
import System.IO(hPutStrLn, stderr)

import Control.Exception
import Data.Maybe(fromJust)

----------------------
-- helper type: a MonadPlus instance defined to die a painful death,
--              like IO but not nearly as annoying to use

data Inhabited a = Inhabited a deriving (Show, Eq, Ord, Functor)
unwrap :: Inhabited a -> a
unwrap (Inhabited a) = a

instance Applicative Inhabited where
	pure = Inhabited
	(Inhabited f) <*> (Inhabited a) = Inhabited (f a)

instance Monad Inhabited where
    a >>= f = unwrap $ f <$> a

instance Alternative Inhabited where
	empty = error "mzero: Inhabited"
	(<|>) = const

instance MonadPlus Inhabited where
	mzero = error "mzero: Inhabited"
	mplus = const

----------------------

isCoprime :: (Integral a) => a -> a -> Bool
isCoprime a b = gcd a b == 1

isAbcSolution :: (Integral a) => (a,a,a) -> (a,a,a) -> Bool
isAbcSolution (α,β,γ) (a,b,c) = α*a*a + β*b*b == γ*c*c

mtrySolveAbcForA :: (Integral a, MonadPlus m) => (a,a,a) -> (a,a) -> m a
mtrySolveAbcForA (α,β,γ) (b,c) = do let αaa = γ*c*c - β*b*b
                                    return αaa >>= (`mtryDiv` α) >>= mtrySqrt

-- a^2 + β b^2 == k c^2
data Solution = Solution Int             -- β:  diophantine coefficient derived from cell symmetry
                         (Int,Int,Int)   -- (a,b,c):  diophantine equation solution
                         (SqrtRatio Int) -- ((n/d) * (sqrt k)):  layer 2 scale factor
                         deriving (Show, Eq, Generic)

instance Aeson.FromJSON Solution
instance Aeson.ToJSON Solution where
	toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

solutionScaleRatio :: Solution -> SqrtRatio Int
solutionScaleRatio (Solution _ _ r) = r
solutionAngle :: Solution -> Double
solutionAngle (Solution β (a,b,c) _) = atan2
                             (SqrtRatio.approxDouble (SqrtRatio.fromParts b c β))
                             (SqrtRatio.approxDouble (SqrtRatio.fromParts a c 1))

solutionVolumes :: Solution -> (Int, Int)
solutionVolumes (Solution β (a,b,c) (SqrtRatio.Raw n d k)) = (cVolume, dVolume) where
	dVolume = analyticDVolume $ (((n % (d*c)) *) . fromIntegral) <$> [a, b, -β*b, a]
	cVolume = analyticDVolume $ (((d % (n*c*k)) *) . fromIntegral) <$> [a, -b, β*b, a]

solutionVolume :: Solution -> Int
solutionVolume = uncurry max . solutionVolumes

asSolution :: (MonadPlus m) => Int -> (Int,Int,Int) -> (Int,Int,Int) -> m Solution
asSolution β (a,b,c) (n,d,k) = do
	unless (isSquarefree β) $ error "fail 1"
	unless (isSquarefree k) $ error "fail 2"
	unless (gcd n d == 1) $ error "fail 3"
	unless (gcd a b == 1) $ error "fail 4"
	unless (gcd a c == 1) $ error "fail 5"
	unless (isAbcSolution (1,β,k) (a,b,c)) $ error "fail 6"
	return $ Solution β (a,b,c) (SqrtRatio.fromParts n d k)

--bruteForceVolume :: [Ratio Integer] -> Integer
bruteForceVolume [e11, e12, e21, e22] = d11*d22 where
    d11 = lcm (denominator e11) (denominator e12)
    (d21,d22) = head $ [ (d21,d22)
                       | d22 <- [1..]
                       , d21 <- [0..d11-1]
                       , denominator ((d21%1)*e11 + (d22%1)*e21) == 1
                       , denominator ((d21%1)*e12 + (d22%1)*e22) == 1
                       ]

assertEqual :: (Show a, Eq a) => a -> a -> a
assertEqual a b | a /= b = error $ show a ++ " /= " ++ show b
                | otherwise = a

gcdDecomp :: Integral x => x -> x -> (x, (x,x))
gcdDecomp a b = (g, (a `quot` g, b `quot` g)) where g = gcd a b

analyticDVolume :: Integral x => [Ratio x] -> x
analyticDVolume eMat = x11*x22 where [x11,_,_,x22] = hnfDMatrix eMat

hnfDMatrix :: Integral x => [Ratio x] -> [x]
hnfDMatrix [ e11@(n11 :% d11), e12@(n12 :% d12)
           , e21@(n21 :% d21), e22@(n22 :% d22) ] = [x11,0,x21,x22] where
    -- cancel GCDs from columns first, THEN the rows.
    (_, (d11',  d21')) = gcdDecomp d11 d21
    (_, (d12',  d22')) = gcdDecomp d12 d22
    (_, (d11'', d12'')) = gcdDecomp d11' d12'
    (_, (d21'', d22'')) = gcdDecomp d21' d22'

    -- first row is grade school easy
    x11 = lcm d11 d12

    -- Second row:
    -- It can be shown that x22 must be a multiple of l = lcm (d21', d22').
    -- Specifically, we want the first positive multiple such that there
    -- exists a solution for x21, which must satisfy two congruences of
    -- the form  x21 = b c_k (mod m_k)
    congruences b = map (\(prefactor,m) -> (prefactor*b, m)) pairs where
        pairs = [ (-(fromJust $ n11 `inverseMod` d11) * d22'' * d11' * n21,  d11)
                , (-(fromJust $ n12 `inverseMod` d12) * d21'' * d12' * n22,  d12)
                ]
    -- When the denominator of the determinant is 1, it can be shown that x22 = l
    -- is always a solution (and thus the best one). But in the general case,
    -- finding the best solution (and *proving* it is the best) seems to be tricky.
    --
    -- For now, we'll just brute force check all multiples.
    (x21, x22) = head [ (x21, b * l) | b <- [1..]
                      , let [congr1, congr2] = congruences b
                      , (x21, _) <- toList $ chineseRemainder congr1 congr2
                      ] where l = lcm d21' d22'

-- who knows why invertMod isn't generic...
inverseMod :: (Integral a) => a -> a -> Maybe a
inverseMod _ 1 = Just 0 -- arithmoi returns Nothing in this case, which is just plain wrong
inverseMod a b = fromIntegral <$> (fromIntegral a `invertMod` fromIntegral b)

enum_analytic_1b :: Int -> Int -> [Solution]
enum_analytic_1b β vmax = giantComprehension where
  limit = vmax*vmax

  giantComprehension = [ sol
                       | l <- [1..vmax] -- lcm of c and n
                       , c <- map fromIntegral $ toList $ divisors l
                       , n <- map fromIntegral $ toList $ divisors l
                       , lcm c n == l
                       , let c' = c `div` gcd n c
                       , let ks = squarefrees & takeWhile (\k -> k*l*l < vmax*vmax)
                       , let !_ = length ks
                       , k <- ks
                       , let ds = [1..] & takeWhile (\d -> d*c' < vmax)
                       , let !_ = length ds
                       , d <- ds
                       , gcd n d == 1
                       , let bs = [0..] & takeWhile (\b -> β*b*b <= k*c*c)
                       , let !_ = length bs
                       , b <- bs
                       , gcd b c == 1
                       , a <- mtrySolveAbcForA (1,β,k) (b,c)
                       , let Inhabited sol = asSolution β (a,b,c) (n,d,k)
                       --, solutionVolume sol <= vmax
                       ]

  squarefrees = [1..] & filter isSquarefree
  coprimeTo x = filter (isCoprime x)
  limiting f = takeWhile ((<= limit) . f)


enum_analytic_2 :: Int -> Int -> Double -> Double -> [Solution]
enum_analytic_2 β vmax rExpect rTol = [ sol
                                      | sol@(Solution _ _ r) <- enum_analytic_1b β vmax
                                      , SqrtRatio.approxDouble r <= rExpect + rTol
                                      , SqrtRatio.approxDouble r >= rExpect - rTol
                                      ]

enum_approx_scale_sols :: Int -> Int -> Double -> Double -> [Solution]
enum_approx_scale_sols β vmax rExpect rTol = enum_analytic_2 β vmax rExpect rTol
-- HACK: uses approxDouble on the premise that SqrtRatios have a unique representation,
--       so that equal ratios produce exactly equal doubles.
enum_exact_scale_sols :: Int -> Int -> SqrtRatio Int -> [Solution]
enum_exact_scale_sols β vmax r = enum_approx_scale_sols β vmax (SqrtRatio.approxDouble r) 0
enum_equal_scale_sols :: Int -> Int -> [Solution]
enum_equal_scale_sols β vmax = enum_exact_scale_sols β vmax 1



main :: IO ()
main = do
    args <- getArgs
    when (null args) $
        mapM_ (hPutStrLn stderr) $
            [ "usage: hex-sols BETA MAX_AREA [R_EXPECT R_TOL]"
            , "      BETA (int)      - parameter in  a^2 + BETA b^2 = c^2"
            , "      MAX_AREA        - max area of a solution, in unit cells"
            , "      R_EXPECT, R_TOL - solutions are found with a lattice mismatch"
            , "                        ratio in [R_EXPECT - R_TOL, R_EXPECT + R_TOL]."
            , "                        R_TOL is NOT just a numerical tolerance. This program"
            , "                        performs exact math internally and an R_TOL of 0"
            , "                        'just works'.  (default: R_EXPECT = 1, R_TOL = 0)"
            ]
    unless (null args) $ do
        let beta = read (args !! 0)
        let vmax = read (args !! 1)
        let rExpect = if length args > 2 then read (args !! 3) else 1
        let rTol = if length args > 2 then read (args !! 4) else 0
        mapM_ (BS.putStrLn .Aeson.encode) $ enum_approx_scale_sols beta vmax rExpect rTol
--        $ map (\x -> (x, solutionAngle x * 180 / pi
--                    , (SqrtRatio.approxDouble $ solutionScaleRatio x) - 1
--                    , solutionVolumes x))
        -- $ enum_approx_scale_sols 3 (60) 1.018 (0.01)
