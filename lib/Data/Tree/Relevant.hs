-- | Compute the relevant sub-forests of a tree difference problem.
--
-- Inspired by S. Dulucq and H. Touzet. /Analysis of tree edit
-- distance algorithms/.

module Data.Tree.Relevant where

import           Control.Applicative
import           Data.Monoid
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Data.Vector         (Vector)
import qualified Data.Vector         as V

import           Debug.Trace

import           Prelude             hiding (null)

import           Data.Tree

-- * Decomposition strategies

data Dir = L | R

-- | A cover strategy governs the decomposition of the forests of a
-- tree edit problem.
type Strategy l = Forest l -> Forest l -> Dir

left :: Strategy l
left _ _ = L

right :: Strategy l
right _ _ = R

-- * Relevant forests

-- | Using a strategy, decompose a pair of forests.
relevantForests :: Ord l => Strategy l -> Forest l -> Forest l -> Set (Forest l, Forest l)
relevantForests strat f1 f2 = go f1 f2 mempty
  where
    go f1 f2 done
      | null f1 && null f2 = done
      | (f1, f2) `Set.member` done = done
      | otherwise =
        let done' = Set.insert (f1, f2) done
        in case strat f1 f2 of
          L -> case (uncons f1, uncons f2) of
            (Just (l, g, t), Nothing) ->
              trace "left-left" $
                go (g <> t) f2 done'
            (Nothing, Just (l', g', t')) ->
              trace "left-right" $
                go f1 (g' <> t') done'
            (Just (l, g, t), Just (l', g', t')) ->
              trace "left-both" $
                go (g <> t) f2 (go f1 (g' <> t') (go (n l g) (n l' g') (go t t' done')))
          R -> case (unsnoc f1, unsnoc f2) of
            (Just (t, l, g), Nothing) ->
              trace "right-left" $
                go (t <> g) f2 done'
            (Nothing, Just (t', l', g')) ->
              trace "right-right" $
                go f1 (t' <> g') done'
            (Just (t, l, g), Just (t', l', g')) ->
              trace "right-both" $
                go (t <> g) f2 (go f1 (t' <> g') (go (n l g) (n l' g') (go t t' done')))
    n l c = Forest $ V.singleton (Node l c)

