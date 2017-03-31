{-# LANGUAGE OverloadedStrings #-}
module Main where
import Debug.Trace
import System.Random
import Data.Foldable
import Data.Monoid
import Data.Aeson((.=))
import qualified Data.Aeson as Aeson
import Data.Vector(Vector,(!))
import qualified Data.Vector as Vector
import qualified Data.List as List
import Data.ByteString.Lazy(ByteString)
import qualified Data.ByteString.Lazy as ByteString

type Perm = Vector Int

main' :: ByteString -> ByteString
main' s = output where
	Just input = Aeson.decode s :: Maybe [[[Double]]]
	(dat,perms) = reorder input
	output = Aeson.encode $ Aeson.object
		[ "data" .= dat
		, "perms" .= perms
		, "gamma-ids" .= (argSort <$> perms)
		]

main :: IO ()
main = do --ByteString.interact main'
    -- bands [[1,6,6],[3,6,6],[5,6,6],[6,6,7],[6,6,9]]
    print $ (List.sort $ concat $ concat $ [[[a+b+c | a <- [1,2,3,4,5]]
                     | b <- [10,20,30,40,50]]
                     | c <- [100,200,300,400,500]])
         ==  (List.sort $ concat $ concat $ fst . reorder $
            [[[a+b+c | a <- [1,2,3,4,5]]
                     | b <- [10,20,30,40,50]]
                     | c <- [100,200,300,400,500]])
--main = mapM_ print $ bands [[1,6],[3,6],[6,5],[6,7],[6,9]]

indices :: [a] -> Vector Int
indices xs = Vector.fromList [0 .. length xs - 1]

argSort :: (Foldable t, Ord a) => t a -> Perm
argSort xs = Vector.fromList $ List.sortOn (vs !) [0 .. length xs - 1] where
	vs = Vector.fromList (toList xs)

permute :: [a] -> Perm -> [a]
permute xs perm = Vector.toList $ Vector.backpermute (Vector.fromList xs) perm

scanl2 :: (b -> b -> a -> b) -> b -> b -> [a] -> [b]
scanl2 f b0 b1 [] = [b0,b1]
scanl2 f b0 b1 (a:as) = b0:scanl2 f b1 (f b0 b1 a) as

fixBands :: (Num a, Ord a) => Perm -> [[a]] -> [(Perm, [a])]
fixBands initPerm xs = run (List.sort <$> xs) where
	run xs = scanl2 f (initItem (xs!!0)) (initItem (xs!!1)) (drop 2 xs)
	f prev cur orig = (perm, next) where
		perm = (argSort.argSort) guess
		guess = zipWith (-) ((2*) <$> snd cur) (snd prev)
		next = permute (List.sort orig) perm
	initItem x = (initPerm, permute (List.sort x) initPerm)

reorderChunk :: (Num a, Ord a) => Perm -> [[a]] -> ([[a]], Perm)
reorderChunk initPerm xs = (theBands, finalPerm) where
	fixed = fixBands initPerm xs
	theBands = fmap snd $ fixed
	finalPerm = fst . last $ fixed

reorder :: (Num a, Ord a) => [[[a]]] -> ([[[a]]],[Perm])
reorder chunks = List.unzip $ rec (indices $ head $ head $ chunks) chunks where
	rec initPerm [] = []
	rec initPerm (chunk:chunks) =
		let (fixed, finalPerm) = reorderChunk initPerm chunk
		in  (fixed, initPerm):rec finalPerm chunks

traceShowOn :: (Show b) => (a -> b) -> a -> a
traceShowOn f x = traceShow (f x) x

shape :: [[[a]]] -> (Int,Int,Int)
shape xs = (length xs, length $ head xs, length . head . head $ xs)

makeExample :: IO [[[Double]]]
makeExample =
	fmap (fmap List.sort <$>) .
	fmap (List.transpose <$>) .
	fmap (List.transpose) .
	(sequence . replicate 100) $ makeChunkedData

makeChunkedData :: IO [[Double]]
makeChunkedData = do
	first  <- makeCurveData
	second <- makeCurveData
	let second' = (+ last first) . (subtract $ head second) <$> second
	return [first, second']

randomPoly :: IO (Double -> Double)
randomPoly = do
	a <- randomIO :: IO Double
	b <- randomIO :: IO Double
	c <- randomIO :: IO Double
	x0 <- randomIO :: IO Double
	return $ \x -> let d = (x-x0) in (1-2*a)*d*d + (1-2*b)*d + c

makeCurveData :: IO ([Double])
makeCurveData = do
	poly <- randomPoly
	return $ poly <$> [0, 1/100.. 1]
