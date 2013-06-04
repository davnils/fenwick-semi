module Main where

import qualified Data.FenwickSemi as F
import Data.List
import Data.Semigroup
import System.Exit (exitFailure)
import Test.QuickCheck

flatten tree size = map (`F.prefix` tree) [0..size]
ref list = tail $ scanl (<>) (Sum 0) list
range list = fromIntegral $ length list - 1

checkInsertion :: NonEmptyList Integer -> Bool
checkInsertion nonEmpty = flatten tree (range list) == ref list
  where
  tree = F.fromList list
  list = map Sum $ getNonEmpty nonEmpty

checkUpdate :: NonEmptyList Integer -> Integer -> Property
checkUpdate nonEmpty val = forAll (elements [0..range list]) verifyUpdate
  where
  verifyUpdate elem =
    flatten (F.update elem (Sum val) inputTree) (range list) ==
    ref (genericTake elem list ++ [Sum val] ++ genericDrop (elem + 1) list)

  inputTree = F.fromList list
  list = map Sum $ getNonEmpty nonEmpty

main :: IO ()
main = mapM_ (quickCheckWith stdArgs { maxSuccess = 5000 }) tests
  where
  tests = [property checkInsertion, property checkUpdate]
