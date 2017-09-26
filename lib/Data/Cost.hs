module Data.Cost (
  Cost, fromCost, cost
) where

import Data.Bifunctor

newtype Cost = Cost { fromCost :: Int }
  deriving (Eq, Ord)

cost :: Int -> Cost
cost i | i < 0 = error "Costs must be positive"
       | otherwise = Cost i

instance Show Cost where
  show = show . fromCost

instance Read Cost where
  readsPrec i = map (first Cost) . filter (\(a,r) -> a >= 0) . readsPrec i

instance Monoid Cost where
  mempty = Cost 0
  (Cost a) `mappend` (Cost b) = Cost (a + b)
