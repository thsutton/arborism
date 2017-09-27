-- | Compute the relevant sub-forests of a tree difference problem.
--
-- Inspired by S. Dulucq and H. Touzet. /Analysis of tree edit
-- distance algorithms/.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Tree.Relevant where

import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Set as Set
import Data.Monoid

import Prelude hiding (null)

import Data.Tree

-- * Decomposition strategies

data Dir = L | R

-- | A cover strategy governs the decomposition of the forests of a
-- tree edit problem.
type Strategy l = (Forest l, Forest l) -> Dir

left :: Strategy l
left (_,_) = L

right :: Strategy l
right (_,_) = R

-- * Relevant forests

-- | Using a strategy, decompose a pair of forests.
relevantForests :: Ord l => Strategy l -> (Forest l, Forest l) -> Set (Forest l, Forest l)
relevantForests strat problem@(f1, f2)
  | null f1 && null f2 = mempty
  | otherwise = case strat problem of
      L -> (f1, f2) `Set.insert` case (uncons f1, uncons f2) of
            (Just (l, g, t), Nothing) ->
              (rf (g <> t, f2))
            (Nothing, Just (l', g', t')) ->
              (rf (f1, g' <> t'))
            (Just (l, g, t), Just (l', g', t')) ->
              (rf (g <> t, f2) <> rf (f1, g' <> t') <> rf (n l g, n l' g') <> rf (t, t'))
      R -> (f1, f2) `Set.insert` case (unsnoc f1, unsnoc f2) of
            (Just (t, l, g), Nothing) -> rf (t <> g, f2)
            (Nothing, Just (t', l', g')) -> rf (f1, t' <> g')
            (Just (t, l, g), Just (t', l', g')) ->
              rf (t <> g, f2) <> rf (f1, t' <> g') <> rf (n l g, n l' g') <> rf (t,t')
  where
    n l c = Forest $ V.singleton (Node l c)
    rf = relevantForests strat
