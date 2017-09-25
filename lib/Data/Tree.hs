{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Tree where

import Data.Bifunctor
import Data.Functor
import Data.Monoid
import Data.Semigroup

import Data.Deque

data Tree l = Node { label :: l, children :: (Forest l) }
  deriving (Show, Eq, Ord, Functor, Traversable, Foldable)

newtype Forest l = Forest { trees :: Deque (Tree l) }
  deriving (Show, Eq, Ord, Monoid, Semigroup, Functor, Traversable, Foldable)

nthTree :: Int -> Forest l -> Maybe (Tree l)
nthTree ix (Forest f) = nth ix f

left :: Forest l -> Maybe (Tree l, Forest l)
left (Forest as) = second Forest <$> uncons as

right :: Forest l -> Maybe (Forest l, Tree l)
right (Forest as) = first Forest <$> unsnoc as

-- | A path through a tree or forest.
newtype Path = Path [Int]

follow :: Path -> Tree l -> Maybe l
follow (Path []) (Node l _)           = Just l
follow (Path (child:path)) (Node _ f) = nthTree child f >>= follow (Path path)
