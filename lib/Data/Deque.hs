{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Deque where

import Data.Functor
import Data.Monoid
import Data.Semigroup
--import           Data.Vector    (Vector)
--import qualified Data.Vector    as V

newtype Deque a = Deque { fromDeque :: [a] }
  deriving (Show, Eq, Ord, Monoid, Semigroup, Functor, Traversable, Foldable)

nth :: Int -> Deque a -> Maybe a
nth n _              | n < 0 = Nothing
nth 0 (Deque (a:as)) = Just a
nth n (Deque (a:as)) = nth (n - 1) (Deque as)

cons :: a -> Deque a -> Deque a
cons a (Deque as) = Deque (a:as)

snoc :: Deque a -> a -> Deque a
snoc (Deque as) a = Deque (as ++ [a])

uncons :: Deque a -> Maybe (a, Deque a)
uncons (Deque as) =
  case as of
    []  -> Nothing
    a:r -> Just (a, Deque r)

unsnoc :: Deque a -> Maybe (Deque a, a)
unsnoc (Deque as) =
  case as of
    [] -> Nothing
    r  -> Just (Deque (init r), last r)
