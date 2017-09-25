module Lib
    ( someFunc
    ) where

import Data.Bifunctor
import Data.Functor

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Forest l = [Tree l]

data Tree l = Node { label :: l, children :: Forest l }

uncons :: [a] -> Maybe (a, [a])
uncons (a:as) = Just (a, as)
uncons []     = Nothing

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc as = Just (init as, last as)

untree :: Tree l -> (l, Forest l)
untree (Node l f) = (l, f)

left :: Forest l -> Maybe ((l, Forest l), Forest l)
left = fmap (first untree) . uncons

right :: Forest l -> Maybe (Forest l, (l, Forest l))
right = fmap (second untree) . unsnoc

data Direction = Left | Right

type Strategry l = Forest l -> Forest l -> Direction

