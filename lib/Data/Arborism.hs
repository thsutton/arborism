module Data.Arborism where

import           Data.Monoid
import           Data.Set    (Set)
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- | A 'Tree' is a node labelled with an element of 'sigma'.
data Tree sigma = Node
  { treeLabel    :: sigma
  , treeBranches :: Forest sigma
  }
  deriving (Ord, Eq)

-- | A 'Forest' is an ordered collection of 'Tree's.
newtype Forest sigma = Forest
  { forestTrees :: Vector (Tree sigma)
  }
  deriving (Ord, Eq)

instance Monoid (Forest sigma) where
  mempty = Forest mempty
  (Forest t1) `mappend` (Forest t2) = Forest (t1 `mappend` t2)

empty :: Forest sigma -> Bool
empty (Forest trees) = V.null trees

-- | The number of children a tree has.
degree :: Tree sigma -> Int
degree = V.length . forestTrees . treeBranches

leftMost :: Forest sigma -> Maybe (Tree sigma, Forest sigma)
leftMost (Forest f)
    | V.null f = Nothing
    | otherwise =
        let l = V.head f
            t = V.tail f
        in Just (l, Forest t)

rightMost :: Forest sigma -> Maybe (Forest sigma, Tree sigma)
rightMost (Forest f)
    | V.null f = Nothing
    | otherwise =
        let r = V.last f
            t = V.init f
        in Just (Forest t, r)

-- | Decompose a forest at the root of the left-most tree.
--
-- Returns the left-most tree's root label and branches, and the right trees.
leftRoot :: Forest sigma -> Maybe ((sigma, Forest sigma), Forest sigma)
leftRoot f = (\(l,t) -> ((treeLabel l, treeBranches l), t)) <$> leftMost f

-- | Decompose a forest at the root of the right-most tree.
--
-- Returns the left trees, right-most tree's root label and branches.
rightRoot :: Forest sigma -> Maybe (Forest sigma, (sigma, Forest sigma))
rightRoot f = (\(t,r) -> (t, (treeLabel r, treeBranches r))) <$> rightMost f

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
relevant phi f f'
  -- if F and F' are empty forests, then RF(phi)(F, F') = \null
  | empty f && empty f' = mempty
  -- if phi(F, F')=left, F=l(G)<>T and F' is empty, then RF{phi}(F,F') is
  -- (f, f') `Set.insert` relevant phi (g <> t) f'
  | phi f f' == L && empty f' = mempty
  -- if phi(F, F')=right, F=T<>l(G) and F' is empty, then RF{phi}(F,F') is
  -- (f,f') `Set.insert` relevant phi (t<>g) f'
  | phi f f' == R && empty f' = mempty

