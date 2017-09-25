module ZS where

import Data.Cost
import Data.Tree

-- $ Edits on trees
--
-- The tree edit problem we'll be considering determines the least
-- cost sequence of operations to change one tree into another. The
-- three operations -- relabelling, deleting, or inserting a node --
-- are direct analogues of the operations we've seen before in the
-- string edit distance problem.
--
-- Relabelling a node, like substituting a character, changes a symbol
-- but not the shape of the structure.
--
-- Deleting a node deletes that node and moves it's children into it's
-- place with respect to it's parent and siblings. This leaves most
-- structures associated with the tree unchanged excepting the single
-- deleted node (traversals, branches, etc have one fewer node, all
-- remaining nodes are in the same order, etc.)
--
-- Inserting a node is the mirror image of deleting one. We insert a
-- new node at some point in the sequence of children of some parent
-- and re-parent some possibly empty contiguous subsequence of
-- siblings at the insertion point. As with deletion, this leaves most
-- structures unchanged but for the additional node (traversals,
-- branches, etc. are one node longer, all original nodes are in the
-- same order w.r.t. each other, etc.)

data Operation l
  = Label l l -- ^ Change a label: a(F) -> b(F)
  | Delete l -- ^ Delete a node: a(F1 . b(F) . F2) -> a(F1 . F . F2)
  | Insert l -- ^ Inserting a node: a(F1 . F . F2) -> a(F1 . b(F) . F2)

-- | The label before the operation.
from :: Operation l -> Maybe l
from (Label l _) = Just l
from (Delete l)  = Just l
from (Insert _)  = Nothing

-- | The label after the operation.
to :: Operation l -> Maybe l
to (Label _ l) = Just l
to (Delete _)  = Nothing
to (Insert l)  = Just l

-- | This is a placeholder; we will modify it later to take the
-- context within which an operation occurs.
--
-- In any case, it had better be a distance metric (non-negative, zero
-- at reflexive points, symmetric, and triangle).
gamma :: Operation l -> Cost
gamma (Label l m) = 1
gamma (Delete l)  = 1
gamma (Insert l)  = 2

-- | We then get a distance metric for sequences of operations.
delta :: [Operation l] -> Cost
delta = foldl gamma 0

-- | A mapping between two trees such that:
--
-- For (i,j), 1 <= i <= |T1|, 1 <= j <= |T2|
-- For (i1,j1) and (i2,j2): i1==i2 iff j1==j2 (one-to-one)
-- For (i1,j1) and (i2,j2): T1[i1] leftOf T1[i2] iff T2[j1] leftOf T2[j2] (sibling order preserved)
-- For (i1,j1) and (i2,j2): T1[i1] ancestorOf T2[i2] iff T2[j1] ancestorOf T2[j2] (ancestor order preserved)
type Mapping l = (Set (Int, Int), Tree l, Tree l)
