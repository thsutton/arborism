{-# LANGUAGE StandaloneDeriving #-}
-- | A simple rose tree with ordered siblings.
module Data.Tree where

import           Control.Arrow (first, second)
import           Data.Functor
import           Data.Monoid
import           Data.Vector   (Vector)
import qualified Data.Vector   as V

-- $setup
--
-- TODO: Useful Arbitrary instances!
--
-- >>> :set -XScopedTypeVariables
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary a => Arbitrary (Forest a) where arbitrary = pure (Forest mempty)
-- >>> instance Arbitrary a => Arbitrary (Tree a) where arbitrary = Node <$> arbitrary <*> arbitrary

-- * Trees and Forests

-- | A 'Tree' is a node labelled with an element of 'sigma'.
data Tree sigma = Node
  { treeLabel    :: sigma
  , treeBranches :: Forest sigma
  }
  deriving (Ord, Eq)

deriving instance (Show sigma) => Show (Tree sigma)

-- | A 'Forest' is an ordered collection of 'Tree's.
newtype Forest sigma = Forest
  { forestTrees :: Vector (Tree sigma)
  }
  deriving (Ord, Eq)

deriving instance (Show sigma) => Show (Forest sigma)

instance Monoid (Forest sigma) where
  mempty = Forest mempty
  (Forest t1) `mappend` (Forest t2) = Forest (t1 `mappend` t2)

-- * Construction

-- | Construct a one-node 'Tree'.
singleton :: sigma -> Tree sigma
singleton l = Node l mempty

-- | Graft a 'Tree' into a 'Forest' on the left.
--
-- prop> \t f -> leftMost (graftLeft t f) == Just (t, f)
graftLeft :: Tree sigma -> Forest sigma -> Forest sigma
graftLeft t (Forest ts) = Forest (t `V.cons` ts)

-- | Graft a 'Tree' into a 'Forest' on the right.
--
-- prop> \t f -> rightMost (graftRight f t) == Just (f, t)
graftRight :: Forest sigma -> Tree sigma -> Forest sigma
graftRight (Forest ts) t = Forest (ts `V.snoc` t)

-- * Predicates and accessors

-- | Check that a 'Forest' is empty.
empty :: Forest sigma -> Bool
empty (Forest trees) = V.null trees

-- | The number of children a tree has.
degree :: Tree sigma -> Int
degree = V.length . forestTrees . treeBranches

-- | The number of nodes in a 'Tree'.
treeNodes :: Tree sigma -> Int
treeNodes (Node _ bs) = 1 + forestNodes bs

-- | The number of nodes in a 'Forest'.
forestNodes :: Forest sigma -> Int
forestNodes (Forest ts) = V.foldl (\c t -> c + treeNodes t) 0 ts

-- * Destruction

-- | Split a 'Tree' into it's root label and a 'Forest' of branches.
--
-- prop> \(l :: Int) -> splitTree (singleton l) == (l, mempty)
-- prop> \(l :: Int) f -> splitTree (Node l f) == (l, f)
splitTree :: Tree sigma -> (sigma, Forest sigma)
splitTree (Node l b) = (l, b)


-- | Get the left-most 'Tree' in a 'Forest'.
leftMost :: Forest sigma -> Maybe (Tree sigma, Forest sigma)
leftMost (Forest f)
    | V.null f = Nothing
    | otherwise =
        let l = V.head f
            t = V.tail f
        in Just (l, Forest t)

-- | Get the right-most 'Tree' in a 'Forest'.
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
--
-- prop> \f t -> leftRoot (graftLeft t f) == Just (splitTree t, f)
leftRoot :: Forest sigma -> Maybe ((sigma, Forest sigma), Forest sigma)
leftRoot f = (first splitTree) <$> leftMost f

-- | Decompose a forest at the root of the right-most tree.
--
-- Returns the left trees, right-most tree's root label and branches.
-- prop> \f t -> rightRoot (graftRight f t) == Just (f, splitTree t)
rightRoot :: Forest sigma -> Maybe (Forest sigma, (sigma, Forest sigma))
rightRoot f = (second splitTree) <$> rightMost f
