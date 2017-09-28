{-# LANGUAGE PackageImports #-}
module Main where

import           Data.Function
import           Data.List
import qualified Data.Set      as Set

import "arborism" Data.Tree
import Data.Tree.Diff
import Data.Tree.Relevant

sz (a, b) = forestSize a + forestSize b

main :: IO ()
main = do
  let fs = relevantForests left (singletonForest t1) (singletonForest t2)
  putStr . unlines . map show . sortBy (flip compare `on` sz) $ Set.toList fs
