module FenwickSemi where

import Control.Arrow ((***))
import Data.Semigroup

data Fenwick a
  = Branch a Integer (Fenwick a) (Fenwick a)
  | Leaf a
  deriving (Eq, Show)

type Index = Integer

fromList :: Semigroup a => [a] -> Fenwick a
fromList = takeTree . go
  where
  takeTree (_, _, t) = t

  go :: Semigroup a => [a] -> (a, Integer, Fenwick a)
  go [single] = (single, 1, Leaf single)
  go l = (sum, leafs, Branch sum leafs t1 t2)
    where
    sum = s1 <> s2
    leafs = c1 + c2
    ((s1, c1, t1), (s2, c2, t2)) = go *** go $ splitAt half l
    half = ceiling $ realToFrac (length l) / realToFrac 2

update :: Semigroup a => Index -> a -> Fenwick a -> Fenwick a
update idx new tree = go path
  where
  path = traverseSeq idx tree

  go [(Leaf _, _)] = Leaf new
  go ((Branch _ count t1 _, RightBranch) : t) = rebuild t1 (go t) count t
  go ((Branch _ count _ t2, LeftBranch) : t) = rebuild (go t) t2 count t
  
  rebuild t1 t2 count t =
    Branch (extractValue t1 <> extractValue t2) count t1 t2

extractValue :: Fenwick a -> a
extractValue (Leaf val) = val
extractValue (Branch val _ _ _) = val

data Direction
  = LeftBranch
  | RightBranch
  | NoBranch
  deriving (Eq, Show)

traverseSeq :: Index -> Fenwick a -> [(Fenwick a, Direction)]
traverseSeq _ l@(Leaf _) = [(l, NoBranch)]
traverseSeq idx b@(Branch _ leafs t1 t2)
  | idx < half = (b, LeftBranch)  : traverseSeq idx t1
  | otherwise  = (b, RightBranch) : traverseSeq (idx - half) t2
  where
  half = ceiling $ realToFrac leafs / realToFrac 2

prefix :: Semigroup a => Index -> Fenwick a -> a
prefix idx tree = go path
  where
  path = traverseSeq idx tree

  go [(Leaf val, _)] = val
  go ((Branch val _ t1 _, RightBranch) : t) = extractValue t1 <> go t
  go (_ : t) = go t
