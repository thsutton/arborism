{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module Data.Tree.Diff where

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Bifunctor
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Data.List

import Data.Tree

-- * Flattened Trees

-- $ The algorithm assigns a number to each node in the tree in
-- left-to-right post-order traversal and uses these to select
-- sub-forests as the sub-problems. Throughout what follows we'll call
-- this number the /index/.

newtype Idx = Idx { fromIdx :: Int }
  deriving (Eq, Ord, Num, Enum)

instance Show Idx where
  show = show . fromIdx

instance Read Idx where
  readsPrec i = map (first Idx) . filter (\(a,r) -> a >= 0) . readsPrec i

data FNode l = N
  { label :: l
  , index :: Idx
  , leftMostChild :: Idx
  , parent :: Idx
  }
  deriving (Show)

-- | An array stored ascending by index.
--
-- The root of the tree is the last node.
newtype Flattened l = F (Vector (FNode l))
  deriving (Show)

-- | Find the root node
root :: Flattened l -> FNode l
root (F v) = V.last v

-- | Find the span of the sub-tree rooted at a node.
tree :: Flattened l -> Idx -> (Idx, Idx)
tree t id = ((leftMostChild $ t `node` id), id)

-- | We'll flatten the tree in a left-right post-order traversal.
flattenTree :: Tree l -> Flattened l
flattenTree tree =
  let flat = flip evalState 0 . (fmap fst) . go $ tree
      root = V.last flat
      root' = root{parent = index root}
  in F $ (V.init flat) `V.snoc` root'
  where
    go :: Tree l -> State Idx (Vector (FNode l), Idx)
    go (Node l (Forest cs)) = do
      cs' <- V.mapM go cs
      next <- state (\next -> (next, next + 1))
      let left = maybe next snd $ cs' V.!? 0
      return $ ((V.concatMap (fixup next) cs') `V.snoc` (N l next left (-1)), left)
    -- I should probably figure out how to tie this knot correctly.
    fixup :: Idx -> (Vector (FNode l), Idx) -> Vector (FNode l)
    fixup p = V.map (\n -> if parent n < 0 then n{parent = p} else n) . fst

-- | We access the elements of a flattened tree safely.
--
-- If we ever get a Nothing: 1) our program is wrong, 2) the answer
-- will be wrong, 3) screw you guys, I'm going home.
node :: Flattened l -> Idx -> FNode l
node (F v) (Idx n) =
  case v V.!? n of
    Nothing -> error $ "Cannot get node " <> show n <> " from tree with " <> show (V.length v) <> " nodes"
    Just v -> v

-- | The key to this algorithm (and the newer better algorithms in the
-- same class) is in identifying the subset of sub-trees in that we
-- must process. This allows us to prune the search space considerably.
--
-- keyRoots(T) := { k | there is k' > k such that l(k) = l(k') }
--
-- i.e. k is either the root of T or it has a left-sibling (i.e. it's
-- own left-most child is different from it's parent's).
keyRoots :: Flattened l -> [Idx]
keyRoots tree@(F v) =
  map index . filter isKeyroot . V.toList $ v
  where
    isKeyroot :: FNode l -> Bool
    isKeyroot n = isRoot n || isKey n
    isRoot :: FNode l -> Bool
    isRoot node = index node == (index . root $ tree)
    isKey :: FNode l -> Bool
    isKey k =
      let lk = leftMostChild k
          lpk = leftMostChild (tree `node` (parent k))
      in lk /= lpk

-- * Tree Distance

-- $ The algorithm proceeds by building a series of tableaux of
-- solutions to two types of sub-problems:
--
-- 1. a tableaux of sub-tree vs sub-tree distances; and
--
-- 2. a series of tableaux of sub-forest vs sub-forest distances.
--

-- | The edit operations.
--
-- These could be enhanced with a path to produce not just an optimal
-- edit distance but also a diff.
data Op l = Ins l
          | Del l
          | Rep l l

-- | We evaluate the cost of an operation.
--
-- We assign equal costs to each operation and we don't use the labels
-- in determining the cost of an operation. It would be entirely
-- sensible for us to use the label information
cost :: Eq l => Op l -> Int
cost (Ins _) = 1
cost (Del _) = 1
cost (Rep f t) | f == t = 0
               | otherwise = 1

treeDist in1 in2 =
  let t1 = flattenTree in1
      t2 = flattenTree in2
      kr1 = keyRoots t1
      kr2 = keyRoots t2

      -- | We'll describe the sub-problems to be evaluated from each
      -- pair of key roots. We can calculate these descriptions easily
      -- but we'll need to normalise them to have a single
      -- representation of the "empty" span.
      dep ((li, i), (lj, j), c) =
        let i' = if i < li then -1 else i
            j' = if j < lj then -1 else j
        in ((li, i'), (lj, j'), c)

      -- Solve a forest vs forest sub-problem.
      trees prev (i, j) =
        let li = leftMostChild $ t1 `node` i
            lj = leftMostChild $ t2 `node` j

            zeros   = [ ((li, -1), (lj, -1), []) ]
            deletes = [ ((li, m), (lj, -1), [
                            dep ((li, m-1), (lj, -1), 1)
                        ])
                      | m <- [ li .. i ]
                      ]
            inserts = [ ((li, -1), (lj, n), [
                            dep ((li, -1), (lj, n-1), 1)
                        ])
                      | n <- [ lj .. j ]
                      ]
            changes = [ ((li, m), (lj, n), if (li1 == li) && (lj1 == lj)
                          then [
                            dep ((li, m-1), (lj, n), cost (Del v1)),
                            dep ((li, m), (lj, n-1), cost (Ins v2)),
                            dep ((li, m-1), (lj, n-1), cost (Rep v1 v2))
                            ]
                          else
                            [
                              dep ((li, m-1), (lj, n), cost (Del v1)),
                              dep ((li, m), (lj, n-1), cost (Ins v2)),
                              dep ((li, li1-1), (lj, lj1-1), treedist prev m n)
                            ])
                      | m <- [ li .. i ]
                      , n <- [ lj .. j ]
                      , let li1 = leftMostChild (t1 `node` m)
                            lj1 = leftMostChild (t2 `node` n)
                            v1 = label (t1 `node` m)
                            v2 = label (t2 `node` n)
                      ]
        in foldl (forests i j) prev $ zeros <> deletes <> inserts <> changes

      treedist prev i1 j1 =
        let key = (tree t1 i1, tree t2 j1)
        in case M.lookup key prev of
             Nothing -> error $ "Cannot find previous subtree: " <> show key
             Just v -> v -- error $ "Found " <> show v <> " for subtree: " <> show key
      forests i j prev subproblem@((li, i1), (lj, j1), cs)
        | i1 == -1 && j1 == -1 = M.insert ((li, -1), (lj, -1)) 0 prev
        | null cs = error $ "Expected to compute minimum but not children given: " <> show subproblem
        | otherwise =
            let f (from, to, c') =
                  case M.lookup (from, to) prev of
                    Just c -> c + c'
                    Nothing -> error $ "Missing subproblem: " <> show (from, to)
            in M.insert ((li, i1), (lj, j1)) (minimum $ map f cs) prev


      -- The forest vs forest sub-problems.
      ftabs = [ (r1, r2) | r1 <- kr1, r2 <- kr2 ]
  in foldl trees mempty ftabs

n :: l -> [Tree l] -> Tree l
n l cs = Node l (Forest $ V.fromList cs)

t1 =
  n 'f'
    [ n 'd'
      [ n 'a' []
      , n 'c'
        [ n 'b' []
        ]
      ]
    , n 'e' []
    ]

t2 =
  n 'f'
    [ n 'c'
      [ n 'd'
        [ n 'a' []
        , n 'b' []
        ]
      ]
    , n 'e' []
    ]
