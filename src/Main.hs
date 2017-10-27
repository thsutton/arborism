{-# LANGUAGE PackageImports #-}
module Main where

import           Data.Function
import           Data.List
import           Data.Monoid
import qualified Data.Set      as Set
import System.Environment

import "arborism" Data.Tree
import Data.Tree.Diff
import Data.Tree.Relevant

sz (a, b) = forestSize (a <> b)

writeOut :: Show l => String -> (Forest l, Forest l) -> IO ()
writeOut name forest@(f1,f2) =
  writeFile ("diagrams/" <> hash forest <> ".dot") (dot $ f1 <> f2)

strat "left" = left
strat "right" = right
strat _ = error "Unknown strategy: use left or right"

main :: IO ()
main = do
  [arg] <- getArgs
  let f1 = singletonForest t1
  let f2 = singletonForest t2
  let fs = relevantForests (strat arg) f1 f2
  let forests = sortBy (flip compare `on` sz) $ Set.toList fs
  mapM_ (writeOut arg) forests
