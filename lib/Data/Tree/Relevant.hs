-- | Compute the relevant sub-forests of a tree difference problem.
--
-- Inspired by S. Dulucq and H. Touzet. /Analysis of tree edit
-- distance algorithms/.

module Data.Tree.Relevant where

import           Control.Applicative
import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Digest.Pure.MD5       as MD5
import           Data.Monoid
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Vector                (Vector)
import qualified Data.Vector                as V

import Debug.Trace

import Prelude hiding (null)

import Data.Tree

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

hash :: Show l => l -> String
hash = show . MD5.md5 . BS.pack . show

-- | Using a strategy, decompose a pair of forests.
relevantForests :: (Show l, Ord l) => Strategy l -> Forest l -> Forest l -> Set (Forest l, Forest l)
relevantForests strat f1 f2 = go f1 f2 (Set.singleton (mempty, mempty))
  where
    msg c (f1, f2) fs =
      let p = "\"" <> hash (f1, f2) <> show " -> "
          q = show " [ label=" <> c <> "\" ];"
      in unlines . map (\c -> p <> hash c <> q) $ fs
    go f1 f2 done
      | null f1 && null f2 = done
      | (f1, f2) `Set.member` done = done
      | otherwise =
        let done' = Set.insert (f1, f2) done
        in case strat f1 f2 of
          L -> case (uncons f1, uncons f2) of
            (Just (l, g, t), Nothing) ->
              trace (msg "L-left" (f1, f2) [(g <> t, f2)]) $
                go (g <> t) f2 done'
            (Nothing, Just (l', g', t')) ->
              trace (msg "L-right" (f1, f2) [(f1, g' <> t')]) $
                go f1 (g' <> t') done'
            (Just (l, g, t), Just (l', g', t')) ->
              trace (msg "L-both" (f1, f2) [(g <> t, f2), (f1, g' <> t'), (n l g, n l' g'), (t ,t')]) $
                go (g <> t) f2 (go f1 (g' <> t') (go (n l g) (n l' g') (go t t' done')))
          R -> case (unsnoc f1, unsnoc f2) of
            (Just (t, l, g), Nothing) ->
              trace (msg "R-left" (f1, f2) [(t <> g, f2)]) $
                go (t <> g) f2 done'
            (Nothing, Just (t', l', g')) ->
              trace (msg "R-right" (f1, f2) [(f1, t' <> g')]) $
                go f1 (t' <> g') done'
            (Just (t, l, g), Just (t', l', g')) ->
              trace (msg "R-both" (f1, f2) [(t <> g, f2), (f1, t' <> g'), (n l g, n l' g'), (t, t')]) $
                go (t <> g) f2 (go f1 (t' <> g') (go (n l g) (n l' g') (go t t' done')))
    n l c = Forest $ V.singleton (Node l c)

