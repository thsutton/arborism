{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Tree edit distance algorithm in P. Klein, /Computing the
-- Edit-Distance Between Unrooted Ordered Trees/ in Bilardi et al
-- (Eds): ESA'98, LNCS 1461, pp. 91--102, 1998.

module Klein where

import Data.Bifunctor
import Data.Functor

import Data.Cost
import Data.Tree

-- | Edit operations on @l@-labelled trees.
data Op l
  = ChangeLabel l l -- ^
  | ContractEdge l

-- | "In stating the time bounds for algorithms, we assume that the
-- cost subroutines take constant time"
cost :: Op l -> Cost
cost _ = 1
