module Data.Arborism where

import           Data.Functor
import           Data.Monoid
import           Data.Set     (Set)
import qualified Data.Set     as Set
import           Data.Vector  (Vector)
import qualified Data.Vector  as V

import Data.Tree


-- * Decomposition strategy

-- $ The chief different between the algorithms is the way in which
-- they decompose the 'Forest's being processed. We can abstract this
-- difference as a 'Strategy' which maps a pair of sub-forests to a
-- direction for decomposition.

data Dir = L | R
  deriving (Eq, Show)

-- | Determine which way to decompose 'Forest's.
type Strategy sigma = Forest sigma -> Forest sigma -> Dir

-- | Always decompose to the left.
leftwise :: Strategy sigma
leftwise _ _ = L

-- | Always decompose to the right.
rightwise :: Strategy sigma
rightwise _ _ = R

-- ** Relevant forests

-- $ The relevant forests -- the set of sub-forests which must be
-- considered when solving a particular problem instance -- is
-- determined by the 'Strategy' in use.

-- | Use a 'Strategy' to determine the relevant sub-forests.
relevant
  :: Ord sigma
  => Strategy sigma
  -> Forest sigma
  -> Forest sigma
  -> Set (Forest sigma, Forest sigma)
relevant strategy f f'
    -- Both forests are empty.
    | empty f && empty f' = mempty
    | otherwise =
      case strategy f f' of
        -- f' is empty.
        L | empty f' ->
            case leftRoot f of
              Nothing -> error "This is not possible"
              Just ((l, g), t) -> (f, f') `Set.insert` go (g <> t) f'
        R | empty f' ->
            case rightRoot f of
              Nothing -> error ""
              Just (t, (_, g)) -> (f, f') `Set.insert` go (t <> g) f'
        -- f is empty.
        L | empty f ->
            case leftRoot f' of
              Nothing -> error "This is not possible"
              Just ((_, g'), t') -> (f, f') `Set.insert` go f (g' <> t')
        R | empty f ->
            case rightRoot f' of
              Nothing -> error "This is not possible"
              Just (t', (_, g')) -> (f, f') `Set.insert` relevant strategy f (t' <> g')
        -- f and f' are non-empty.
        L -> case (leftRoot f, leftRoot f') of
          (Just ((l, g), t), Just ((l', g'), t')) ->
            (f, f') `Set.insert` (go (g <> t) f' <> go f (g'<>t') <> go (forest l g) (forest l' g') <> go t t')
          _ -> error "This is not possible"
        R -> case (rightRoot f, rightRoot f') of
          (Just (t, (l, g)), Just (t', (l', g'))) ->
            (f,f') `Set.insert` go (t<>g) f' <> go f (t'<>g') <> go (forest l g) (forest l' g') <> go t t'
  where
    go = relevant strategy
    forest :: sigma -> Forest sigma -> Forest sigma
    forest l g = Forest (V.singleton (Node l g))
