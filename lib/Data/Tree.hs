{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A simple rose tree with ordered siblings.
module Data.Tree where

import           Control.Arrow (first, second)
import           Data.Functor
import           Data.List
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

instance Show l => Show (Tree l) where
  show (Node l (Forest v))
    | V.null v = show l
    | otherwise =
      let atom = show l
          rest = map show $ V.toList v
          words = atom : rest
      in "(" <> unwords words <> ")"

treeSize :: Tree l -> Int
treeSize (Node _ f) = 1 + forestSize f

-- | A 'Forest' is an ordered collection of 'Tree's.
newtype Forest sigma = Forest
  { forestTrees :: Vector (Tree sigma) }
  deriving (Ord, Eq, Monoid)

instance Show l => Show (Forest l) where
  show (Forest children) = "[" <> (unwords . map show . V.toList $ children) <> "]"

forestSize :: Forest l -> Int
forestSize (Forest ts) = V.foldl' (\a t -> a + treeSize t) 0 ts

-- * Construction

-- | Construct a one-node 'Tree'.
singleton :: sigma -> Tree sigma
singleton l = Node l mempty

forest :: [Tree l] -> Forest l
forest l = Forest (V.fromList l)

singletonForest :: Tree l -> Forest l
singletonForest t = Forest (V.singleton t)

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

-- * Maps

type Pos = [Int]

mapForest :: (l -> l') -> Forest l -> Forest l'
mapForest f = mapForestPos (\_ l -> f l)

mapForestPos :: (Pos -> l -> l') -> Forest l -> Forest l'
mapForestPos f = goF []
  where
    goT path (Node l fs) = Node (f path l) (goF path fs)
    goF path (Forest ts) = Forest (V.imap (\ix a -> goT (path <> [ix]) a) ts)

foldLabel :: acc -> (acc -> l -> acc) -> Forest l -> acc
foldLabel acc f = goF acc
  where
    goT acc (Node l fs) = goF (f acc l) fs
    goF acc (Forest ts) = V.foldr (flip goT) acc ts

foldEdges :: acc -> (acc -> l -> l -> acc) -> Forest l -> acc
foldEdges acc f = goF acc
  where
    goT acc (Node p fs@(Forest cs)) = goF (V.foldr (\(Node c _) a -> f a p c) acc cs) fs
    goF acc (Forest ts) = V.foldl goT acc ts

-- | Visit each node and edge.
--
-- We visit, in order: the incoming edge, the node, it's outgoing edges.
foldTree :: acc -> (acc -> l -> acc) -> (acc -> l -> l -> acc) -> Forest l -> acc
foldTree acc node edge = goF acc
  where
    goT acc (Node p fs) = acc
    goF acc (Forest ts) = acc

dot :: Show l => Forest l -> String
dot f =
  let fl = mapForestPos (\p l -> (show . intercalate "." . map show $ p, show $ show l)) f
  in unlines $ [ "digraph {" ] <>
               foldLabel [] (\acc (p, l) -> ("  " <> p <> "[label=" <> l <> "];") : acc ) fl <>
               foldEdges [] (\acc (p, _) (c, _) -> ("  " <> p <> " -> " <> c <> ";") : acc) fl <>
               [ "}" ]

showS :: Forest l -> String
showS = show . mapForestPos (\p _ -> intercalate "." . map show $ p)

-- * Predicates and accessors

-- | Check that a 'Forest' is empty.
null :: Forest sigma -> Bool
null (Forest trees) = V.null trees

-- | The number of children a tree has.
degree :: Tree sigma -> Int
degree (Node _ (Forest branches)) = V.length branches

-- | The number of nodes in a 'Tree'.
treeNodes :: Tree sigma -> Int
treeNodes (Node _ bs) = 1 + forestNodes bs

-- | The number of nodes in a 'Forest'.
forestNodes :: Forest sigma -> Int
forestNodes (Forest ts) = V.foldl (\c t -> c + treeNodes t) 0 ts

-- * Destruction

uncons :: Forest l -> Maybe (l, Forest l, Forest l)
uncons (Forest v)
  | V.null v = Nothing
  | otherwise =
    let (Node l g) = V.head v
        t = Forest $ V.tail v
    in Just (l, g, t)

unsnoc :: Forest l -> Maybe (Forest l, l, Forest l)
unsnoc (Forest v)
  | V.null v = Nothing
  | otherwise =
      let (Node l g) = V.last v
          t = Forest $ V.init v
      in Just (t, l, g)


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
leftRoot f = first splitTree <$> leftMost f

-- | Decompose a forest at the root of the right-most tree.
--
-- Returns the left trees, right-most tree's root label and branches.
-- prop> \f t -> rightRoot (graftRight f t) == Just (f, splitTree t)
rightRoot :: Forest sigma -> Maybe (Forest sigma, (sigma, Forest sigma))
rightRoot f = second splitTree <$> rightMost f

