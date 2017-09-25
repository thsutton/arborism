{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Cost where

import Data.Bifunctor
import Data.Functor

-- | Costs of operations, which we assume to be non-negative.
newtype Cost = Cost { fromCost :: Int }
  deriving (Ord, Eq, Num)

instance Show Cost where
  show = show . fromCost

instance Read Cost where
  readsPrec i = fmap (first Cost) . readsPrec i
