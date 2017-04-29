{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Band.NonzeroDots where

import           "exphp-prelude" ExpHPrelude hiding (putStr)
import           "base" GHC.Generics
import           "base" Data.Monoid(Sum(..))
import           GeneralUtil(reallyAssert)

-------------------

-- NOTE
--  I would prefer this be formulated as a general algorithm on matrices.
--  Some comments have been fixed but most code has not.
--
--  * rename instances of "ket" to "row"
--  * rename instances of "bra" to "col"
--  * similarly, KetLikeId -> RowId, BraLikeId -> ColId
--  * swap order in arguments and output so that row comes before column

-- newtyped indices into the objects that getNonzeroDots works with
-- (which might just be the kets, or could be degenerate subspaces).
newtype BraLikeId = BraLikeId Int deriving (Eq, Show, Ord, Read)
newtype KetLikeId = KetLikeId Int deriving (Eq, Show, Ord, Read)

-- | Given a matrix with the following properties:
--
-- * all values are nonnegative,
-- * the sum of each row is known,
-- * the sum of each column is known,
-- * for any given row or column, all elements >= some threshold lie within a relatively small span
--   (defined as the difference in index between the first and last such elements),
--
-- identify all elements that meet the threshold, without ever computing most elements
--  outside of the aforementioned "relatively small spans".
--
-- The current implementation is limited to finite numbers of rows and columns, but only due to
--  debugging/reporting measures that are not strictly necessary.
getNonzeroDots :: forall b k.
                  Double                  -- threshold
               -> (b -> k -> Double)      -- compute value at position
               -> (b -> Double)           -- sum of a row
               -> (k -> Double)           -- sum of a column
               -> [b]                     -- rows, in order
               -> [k]                     -- columns, in order
               -> (DotSearchSummary, [(BraLikeId, KetLikeId, Double)])
getNonzeroDots threshold computeProb braProb ketProb allBras allKets = result
  where
    result = loopOverKets mempty 0 [] initialBras initialKets & second (>>= itemsForBra)

    initialBras = zipWith (\i b -> BraState (braProb b) True [] i b) (BraLikeId <$> [0..]) allBras
    initialKets = zip (KetLikeId <$> [0..]) allKets

    itemsForBra BraState{braId, braNonzeroDots} = uncurry (flip (braId,,)) <$> braNonzeroDots

    loopOverKets :: DotSearchSummary -> Int -> [BraState b] -- fold state
                 -> [BraState b] -> [(KetLikeId, k)]        -- input
                 -> (DotSearchSummary, [BraState b])
    loopOverKets summary _     out bras [] = (summary, out ++ bras)
    loopOverKets summary ndone out bras (ket:kets') = continue
      where
        continue = loopOverKets summary' ndone' out' bras' kets'

        (thisMaxProb, dotted, skipped) = scanRowWithKet ket bras

        -- FIXME ^ do something with these guys to produce a DotSearchSummary
        actions = getBraActions (dotted, skipped)
        summary' = force summary <> DotSearchSummary
            -- NOTE: the "length skipped" prevents us from supporting infinite # bras
            { dotSearchDotsPerformed = Sum $ length dotted
            , dotSearchSkippedLeft   = Sum $ ndone
            , dotSearchSkippedRight  = Sum $ length skipped
            , dotSearchMaxProb       = thisMaxProb
            }

        justFinished = actions >>= toFinishFromAction

        bras' = actions >>= toKeepFromAction
        ndone' = ndone + length justFinished
        out' = (actions >>= toFinishFromAction) ++ out

    -- returns (thoseDottedWith, thoseShortCircuited)
    scanRowWithKet :: (KetLikeId, k) -> [BraState b] -> (Double, [BraState b], [BraState b])
    scanRowWithKet ket = rec 0 [] (ketProb $ snd ket) where
        rec best done _ [] = (best, reverse done, [])
        rec best done remainProb bras@(bra:rest)
                | remainProb <= threshold = (best, reverse done, bras) -- short-circuit to leave many bras pristine
                | otherwise               = let (prob, bra') = braKetUpdate ket bra
                                            in rec (max prob $! best) (bra' : done) (remainProb - prob) rest

    -- Inspect a matrix element, performing a dot product and updating a bra's state.
    braKetUpdate :: (KetLikeId, k) -> BraState b -> (Double, BraState b)
    braKetUpdate (ketI, ket) (BraState remainingProb _ dotProds braId bra)
        = (thisProb, BraState remainingProb' False dotProds' braId bra)
      where
        thisProb       = bra `computeProb` ket
        remainingProb' = remainingProb - thisProb
        dotProds'      = (thisProb, ketI) : dotProds

    -- Filter out bras we no longer need to track, thus trimming the left edge of the
    --   region of the matrix that we are searching.
    getBraActions :: ([BraState b], [BraState b]) -> [BraAction b]
    getBraActions (dottedBras, skippedBras) = rec dottedBras where
        rec [] = [ShortCircuit skippedBras]
        -- short-circuiting base case dodges accidental O(n^2) filtering.
        -- Notice that it must be the final item produced, so that it contributes
        --  O(1) total cost during the `toKeepFromAction` flat-map operation.
        rec (bra:_) | braIsPristine bra = error "getBraActions: impossible" -- FIXME the pristine flags are pointless now; kill em
        rec (bra:rest) | braStillUseful bra = KeepTracking bra : rec rest
                       | otherwise          = FinishBra    bra : rec rest

    -- It is tempting to stop searching for a bra's partner once the best probability
    --  exceeds the total remaining probability;  but doing so in turn makes it difficult
    --  to know when we're done *with a ket.* Look strictly at total probability instead.
    braStillUseful :: BraState b -> Bool
    braStillUseful = (threshold <=) . braUnusedProb

    toKeepFromAction (KeepTracking bra) = [bra]
    toKeepFromAction (FinishBra _)      = []
    toKeepFromAction (ShortCircuit bras) = bras

    toFinishFromAction (FinishBra bra)  = [bra]
    toFinishFromAction _ = []

data BraAction b = KeepTracking (BraState b)
                 | FinishBra    (BraState b)
                 | ShortCircuit [BraState b]


data BraState b = BraState
    { braUnusedProb  :: Double                -- remaining probability not yet accounted for
    , braIsPristine  :: Bool                  -- have we attempted to dot this with at least one ket?
    , braNonzeroDots :: [(Double, KetLikeId)] -- (prob, ketId) for kets with nonzero overlap
    , braId          :: BraLikeId             -- the bra index
    , braBra         :: b                     -- the bra!
    }

data DotSearchSummary = DotSearchSummary
    { dotSearchDotsPerformed :: Sum Int -- count not skipped
    , dotSearchSkippedLeft   :: Sum Int -- count skipped by being finished
    , dotSearchSkippedRight  :: Sum Int -- count skipped by short circuiting
    , dotSearchMaxProb       :: Double -- maximum probability encountered on entire grid
    } deriving (Generic, Show)

instance NFData DotSearchSummary where
instance Monoid DotSearchSummary where
    mempty = DotSearchSummary 0 0 0 0
    mappend (DotSearchSummary a1 b1 c1 d1) (DotSearchSummary a2 b2 c2 d2)
        = DotSearchSummary (a1 <> a2) (b1 <> b2) (c1 <> c2) (max d1 d2)

dotSearchTotal :: DotSearchSummary -> Int
dotSearchTotal DotSearchSummary{..} =
    let Sum it = dotSearchDotsPerformed <> dotSearchSkippedRight <> dotSearchSkippedLeft
    in it

-- NOTE: any usage of this precludes infinite # kets or bras
exhaustivelyTestPrecondition :: (b -> k -> Double)      -- compute value at position
                             -> (b -> Double)           -- row sum
                             -> (k -> Double)           -- col sum
                             -> [b]                     -- rows in order
                             -> [k]                     -- cols in order
                             -> (a -> a)                -- id or bottom
exhaustivelyTestPrecondition dot braProb ketProb bras kets
    = reallyAssert (all (\bra -> (1e-6>) . abs . (subtract (braProb bra)) . sum . map (\ket -> (bra `dot` ket)) $ kets) bras)
    . reallyAssert (all (\ket -> (1e-6>) . abs . (subtract (ketProb ket)) . sum . map (\bra -> (bra `dot` ket)) $ bras) kets)
