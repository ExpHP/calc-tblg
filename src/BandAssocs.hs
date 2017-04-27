{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module BandAssocs where

-- !!! NOTE: This module has been superceded. !!!

import           "base" Control.Arrow
import           "base" Data.Complex
import           "base" Data.Tuple
import           "base" Data.Ord
import           "base" Data.Foldable
import           "base" Data.Monoid
import           "base" Debug.Trace
import qualified "base" Data.List as List
import           "vector" Data.Vector(Vector,(!))
import qualified "vector" Data.Vector as Vector
import qualified "utility-ht" Data.List.Key as List.Key
import           "containers" Data.Map.Strict(Map)
import qualified "containers" Data.Map.Strict as Map
import           "containers" Data.Set(Set)
import qualified "containers" Data.Set as Set
import           "aeson" Data.Aeson((.=))
import qualified "aeson" Data.Aeson as Aeson
import qualified "vector" Data.Vector as Vector
import           "bytestring" Data.ByteString.Lazy(ByteString)
import qualified "bytestring" Data.ByteString.Lazy as ByteString

-- FIXME because we compute all N^2 magnitudes
--       I don't think there is actually any benefit to the
--       graph component search. It has been disabled.

type Ket = Vector (Complex Double)
type Labeled s x = (s,x)
type BandAssocs k v = (Map k v, Map v k)

assocFwd :: _ => BandAssocs a b -> (a -> b)
assocFwd assocs x = maybe (error "band id") id $ Map.lookup x (fst assocs)
assocRev :: _ => BandAssocs a b -> (b -> a)
assocRev assocs x = maybe (error "band id") id $ Map.lookup x (snd assocs)

assocPermute2ndLike1st :: _ => BandAssocs Int Int -> [a] -> [a]
assocPermute2ndLike1st (fwd,rev) xs =
    case mapM (flip Map.lookup rev) [0..length xs-1] of
        Nothing -> error $ "assocPermute2ndLike1st: Keys must be zero-based integers up to " ++ show (length xs)
        Just idxs -> toList (Vector.fromList xs `Vector.backpermute` Vector.fromList idxs)

-- the only reason this is generic over index types is to let the type system
--  catch more mistakes.  I only plan to use this for integral indices.
bandPerm :: _ => [Labeled s Ket] -> [Labeled t Ket] -> BandAssocs s t
bandPerm ss ts = ( Map.fromList allPairings
                 , Map.fromList (swap <$> allPairings)
                 )
  where
    -- pairings for a block diagonal subspace
    pairings (sc0,tc0) = rec tc0 (originalOrder sc0) where
        -- greedily assign the closest ket to the next bra
        rec tc [] = []
        rec tc (s:sc) = (s,t):rec (Set.delete t tc) sc
          where t = List.Key.minimum (sqmagnitude . (s `getDot`)) (toList tc)

    getDot s t = dot (fromJust $ Map.lookup s bras)
                     (fromJust $ Map.lookup t kets)
      where bras = Map.fromList ss
            kets = Map.fromList ts
            fromJust = maybe (error "poopie") id

    originalOrder sc = filter (`elem` sc) $ fmap fst ss
    allPairings = comps >>= pairings
    -- HACK: disabling bipartiteComponents because it currently
    --       is O(N^2) anyways.  oops.
    --comps = bipartiteComponents $ bandNonzeroPairs 1e-7 ss ts
    comps = [(Set.fromList . fmap fst $ ss, Set.fromList . fmap fst $ ts)]

---------------------------------------------------------------
-- from earlier script bandfix
type Perm = Vector Int

-- indexed by:  hsymline, kpoint, band index
-- reorder band data, untangling band crossings between each high-symmetry point,
-- and making a vague attempt to line up the indices across high-symmetry points.
reorder :: (Num a, Ord a) => [[[a]]] -> ([[[a]]],[Perm])
reorder chunks = List.unzip $ rec (indices $ head $ head $ chunks) chunks where
    rec initPerm [] = []
    rec initPerm (chunk:chunks) =
        let (fixed, finalPerm) = reorderChunk initPerm chunk
        in  (fixed, initPerm):rec finalPerm chunks

-- reorder each chunk separately, without attempting to make bands match
--  up across the high-sym points.
reorderChunks :: (Num a, Ord a) => [[[a]]] -> [[[a]]]
reorderChunks chunks = fmap (fst . reorderChunk (indices $ head $ head chunks)) chunks

reorderV :: (Num a, Ord a) => Vector (Vector (Vector a)) -> Vector (Vector (Vector a))
reorderV = toV . fst . reorder . fromV

toV :: [[[a]]] -> Vector (Vector (Vector a))
toV = Vector.fromList . fmap Vector.fromList . fmap (fmap Vector.fromList)
fromV :: Vector (Vector (Vector a)) -> [[[a]]]
fromV = toList . fmap toList . fmap (fmap toList)

indices :: [a] -> Vector Int
indices xs = Vector.fromList [0 .. length xs - 1]

argSort :: (Foldable t, Ord a) => t a -> Perm
argSort xs = Vector.fromList $ List.sortOn (vs !) [0 .. length xs - 1] where
    vs = Vector.fromList (toList xs)

permute :: [a] -> Perm -> [a]
permute xs perm = Vector.toList $ Vector.backpermute (Vector.fromList xs) perm

-- produces n+2 elements
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

traceShowOn :: (Show b) => (a -> b) -> a -> a
traceShowOn f x = traceShow (f x) x

shape :: [[[a]]] -> (Int,Int,Int)
shape xs = (length xs, length $ head xs, length . head . head $ xs)

--------------------------------------------------------------
-- tsk tsk tsk, all unused due to still being O(N^2)

bandNonzeroPairs :: _
           => Double
           -> [Labeled s Ket]
           -> [Labeled t Ket]
           -> [(s, t)]
bandNonzeroPairs threshold bras kets =
    [ (s,t) | (s,ks) <- bras
            , (t,kt) <- kets
            , sqmagnitude (ks `dot` kt) >= threshold^2 ]

edgesToAdj :: _
           => [(s, s)]
           -> Map s (Set s)
edgesToAdj = rec Map.empty where
    rec m [] = m
    rec m ((s,t):rest)
        = Map.alter (Just . maybe (Set.singleton t) (Set.insert t)) s m

reachableFrom :: _ => s -> Map s (Set s) -> Set s
reachableFrom root g = fixpointEq step (Set.singleton root)
  where
    step = foldr Set.union Set.empty
            . fmap (maybe undefined id . flip Map.lookup g) . Set.toList

fixpointEq :: (Eq a)=> (a -> a) -> a -> a
fixpointEq f = rec where
    rec a = case f a of fa | fa == a -> fa
                           | True    -> fixpointEq f fa

components :: _ => Map s (Set s) -> [Set s]
components g | Map.null g = []
             | True = let c = reachableFrom (fst $ Map.findMin g) g
                      in c:components (g `mapSetDifference` c)

mapSetDifference :: _ => Map a b -> Set a -> Map a b
mapSetDifference map set = foldr Map.delete map (toList set)

bipartiteComponents :: _ => [(s, t)] -> [(Set s, Set t)]
bipartiteComponents = fmap after . components . before
  where
    -- bipartite edges to uniform edges
    before = edgesToAdj . (\es -> es ++ fmap swap es)
                        . fmap (Left *** Right)

    -- uniform components to bipartite components
    after set = (Set.fromList lefts, Set.fromList rights) where
        lefts  = Set.toList set >>= either pure (const [])
        rights = Set.toList set >>= either (const []) pure

sqmagnitude :: (Functor t, Foldable t, Num a)=> t a -> a
sqmagnitude = sum . fmap (^2)

dot :: Ket -> Ket -> Complex Double
dot a b = sum ((*) <$> fmap conjugate a <*> b)

sqnorm :: Ket -> Double
sqnorm = sum . fmap sqmagnitude

norm :: Ket -> Double
norm = sqrt . sqnorm
