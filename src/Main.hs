{-# LANGUAGE PackageImports #-}
module Main where

import           Data.Function
import           Data.List
import           Data.Monoid
import qualified Data.Set      as Set

import "arborism" Data.Tree
import Data.Tree.Diff
import Data.Tree.Relevant

sz (a, b) = forestSize (a <> b)

writeOut name forest@(f1,f2) =
  writeFile ("diagrams/" <> hash forest <> ".dot") (dot $ f1 <> f2)

main :: IO ()
main = do
  let f1 = singletonForest t1
  let f2 = singletonForest t2
  let fs = relevantForests left f1 f2
  putStrLn (show $ f1 <> f2)
  putStrLn (dot $ f1 <> f2)
  putStrLn ""
  let forests = sortBy (flip compare `on` sz) $ Set.toList fs
  mapM_ (uncurry writeOut) (zip [1..] forests)

  putStrLn $ hash (f1, f2)
