module Data.FenwickSemi
(FenwickSemi(..), fromList, update, prefix)
where

import Control.Arrow ((***))
import Data.Semigroup

-- | (Binary) Fenwick tree ordered according to some input indexing.
data FenwickSemi a
  -- | Intersection of two branches, which also stores an accumulator and the number of leafs.
  = Branch a Integer (FenwickSemi a) (FenwickSemi a)
  -- | Leaf containing an input value.
  | Leaf a
  deriving (Eq, Show)

-- | Type of input indices.
type Index = Integer

-- | Build a Fenwick tree given an input list.
--   Will throw an exception if given an empty list (only source of exceptions).
fromList :: Semigroup a => [a] -> FenwickSemi a
fromList [] = error "fenwick-semi: Can't construct empty tree"
fromList l = takeTree $ go l
  where
  takeTree (_, _, t) = t

  go :: Semigroup a => [a] -> (a, Integer, FenwickSemi a)
  go [single] = (single, 1, Leaf single)
  go l = (sum, leafs, Branch sum leafs t1 t2)
    where
    sum = s1 <> s2
    leafs = c1 + c2
    ((s1, c1, t1), (s2, c2, t2)) = go *** go $ splitAt half l
    half = ceiling $ realToFrac (length l) / realToFrac 2

-- | Update some input value and rebuild the tree structure.
update :: Semigroup a => Index -> a -> FenwickSemi a -> FenwickSemi a
update idx new tree = go path
  where
  path = traverseSeq idx tree

  go [(Leaf _, _)] = Leaf new
  go ((Branch _ count t1 _, RightBranch) : t) = rebuild t1 (go t) count t
  go ((Branch _ count _ t2, LeftBranch) : t) = rebuild (go t) t2 count t
  
  rebuild t1 t2 count t =
    Branch (extractValue t1 <> extractValue t2) count t1 t2

-- | Extract the top-most value in a tree, i.e. either an accumulator or a leaf value.
extractValue :: FenwickSemi a -> a
extractValue (Leaf val) = val
extractValue (Branch val _ _ _) = val

-- | Direction type recording which branch was taken.
data Direction
  -- | The left branch was taken.
  = LeftBranch
  -- | The right branch was taken.
  | RightBranch
  -- | No direction available (leaf node).
  | NoBranch
  deriving (Eq, Show)

-- | Traverse the tree top-down to some leaf which is identified by an index.
--   Records the path taken with all branches and the final leaf node.
traverseSeq :: Index -> FenwickSemi a -> [(FenwickSemi a, Direction)]
traverseSeq _ l@(Leaf _) = [(l, NoBranch)]
traverseSeq idx b@(Branch _ leafs t1 t2)
  | idx < half = (b, LeftBranch)  : traverseSeq idx t1
  | otherwise  = (b, RightBranch) : traverseSeq (idx - half) t2
  where
  half = ceiling $ realToFrac leafs / realToFrac 2

-- | Prefix accumulator operation, calculates the prefix up to (inclusive) some index.
prefix :: Semigroup a => Index -> FenwickSemi a -> a
prefix idx tree = go path
  where
  path = traverseSeq idx tree

  go [(Leaf val, _)] = val
  go ((Branch val _ t1 _, RightBranch) : t) = extractValue t1 <> go t
  go (_ : t) = go t
