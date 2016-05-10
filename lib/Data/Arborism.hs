module Data.Arborism where

import           Data.Vector (Vector)
import qualified Data.Vector as V

-- | A 'Tree' is a labelled node with children.
data Tree sigma = Node
  { nodeLabel    :: sigma
  , nodeChildren :: Forest sigma
  }

newtype Forest sigma = Forest
  { forestTrees :: Vector (Tree sigma)
  }
