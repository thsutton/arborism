{-# LANGUAGE RecordWildCards #-}
module Data.Tree.Diff where

import Data.Tree
import Data.Cost

type Diff l = [Op l]

type Path = [Int]

withPath :: (Path -> Path) -> Op l -> Op l
withPath f op = op{opPath = f (opPath op)}

data Op l
  = Relabel { opPath :: Path, relabelFrom :: l, relabelTo :: l }
  | Delete { opPath :: Path, deleteLabel :: l }
  | Insert { opPath :: Path, insertLabel :: l }

delta :: Op l -> Cost
delta _ = cost 1

treeDiff :: Eq l => Tree l -> Tree l -> Diff l
treeDiff t1 t2 = map (withPath tail) $ forestDiff (singletonForest t1) (singletonForest t2)

forestDiff :: Eq l => Forest l -> Forest l -> Diff l
forestDiff _ _ = error "Unimplemented"
