module Main (main) where

import Language.Haskell.HLint (hlint)
import System.Exit            (exitFailure, exitSuccess)

arguments :: [String]
arguments =
    [ "lib"
    , "test"
    , "src"
    ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints
    then exitSuccess
    else exitFailure
