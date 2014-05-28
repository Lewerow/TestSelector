-----------------------------------------------------------------------------
--
-- Module      :  ParetoSearch
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -F -pgmF htfpp #-}
module ParetoSearch (
  paretoSet,
  Coverage, covers, uses ,
  Paretian, isObviouslyBetterThan, isWorthMergingWith,
  htf_thisModulesTests
) where

-- parse
-- filter worse
-- evaluate options
-- create map
-- if is pareto
-- if is better than single element
-- remove single from map
-- remove elements containing it from pareto
-- add to pareto
-- do next

import Prelude
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe(fromJust)
import Test.Framework
import Test.HUnit

class Paretian a where
--  isObviouslyBetterThan:: a -> a -> Ordering
  isObviouslyBetterThan:: a -> a -> Bool
  isWorthMergingWith:: a -> a -> Bool

class Coverage a where
  covers:: a -> a -> Bool
  uses:: a -> a -> Bool

-- TESTS -----------------
instance Paretian Int where
--  isObviouslyBetterThan a b = if a > (b * b) then GT else if a < b then LT else EQ
  isObviouslyBetterThan a b = a > (b * b)
  isWorthMergingWith a b = a < b

test_filter = do assertEqual (S.fromList ([3,4,5] :: [Int])) (filterMuchWorse (S.fromList ([1,2,3,4,5] :: [Int])))

instance Coverage Int where
  covers a b = a > 2 * b
  uses a b = False

test_makeOptions = do assertEqual expect (makeOptions $ S.fromList ([1,2,0,5] :: [Int])) where
  expect = [(0, S.fromList [1,2,5]), (1, S.fromList [2, 5]), (2, S.fromList [5]), (5, S.empty)] :: [(Int, S.Set Int)]

instance Coverage a => Coverage (Sum a) where
  a `covers` b = getSum a `covers` getSum b
  a `uses` b = getSum a `uses` getSum b

test_spawnOptions = do assertEqual expect actual where
  expect = [(Sum 1, S.fromList [Sum 2, Sum 3]), (Sum 2, S.fromList [Sum 3])] :: [(Sum Int, S.Set (Sum Int))]
  actual = spawnOptions map (Sum 0, S.fromList [Sum 1, Sum 2, Sum 3]) where
    map = M.fromList [(Sum 1, S.fromList [Sum 2, Sum 3, Sum 4]), (Sum 2, S.fromList [Sum 3, Sum 4])]

instance (Ord a, Paretian a) => Paretian (Sum a) where
  isObviouslyBetterThan a b = isObviouslyBetterThan (getSum a) (getSum b)
  isWorthMergingWith a b = (getSum a) < (getSum b)

test_paretoFinder = do assertEqual expected actual where
  expected = S.fromList [Sum 5, Sum 3] :: S.Set (Sum Int)
  actual = S.fromList $ paretoSet [Sum 1, Sum 2, Sum 3]


-- REAL CODE -------------
filterMuchWorse:: Paretian a => Ord a => S.Set a -> S.Set a
filterMuchWorse set = S.foldl' (\rest current -> S.filter (\x -> not (current `isObviouslyBetterThan` x)) rest) set set

makeOptions:: Paretian a => Ord a => S.Set a -> [(a, S.Set a)]
makeOptions set = map (\x -> (x, S.filter (\y -> x `isWorthMergingWith` y) set)) $ S.toList set

spawnOptions:: Coverage a => Monoid a => Ord a => M.Map a (S.Set a) -> (a, S.Set a) -> [(a, S.Set a)]
spawnOptions bindings test = map (createRecord test) $ S.toList $ S.filter (flip M.member bindings) $ snd test where
  createRecord test t = let v = fst test <> t in (v, opts v) where
    opts v = S.filter (\x -> not (v `covers` x)) $ S.intersection (snd test) (fromJust $ M.lookup t bindings)

findParetoHelper:: Paretian a => Coverage a => Monoid a => Ord a => (a, S.Set a) ->  M.Map a (S.Set a) -> [(a, S.Set a)] -> [a] -> [a]
findParetoHelper current bindings remaining results = findPareto bindings (newAvailable remaining) newResults where
  newAvailable available = (spawnOptions bindings current) ++ available
  newResults = fst current : (filter  (removeWorse $ fst current) results) where
    removeWorse current x = not (current `isObviouslyBetterThan` x)

findPareto:: Paretian a => Coverage a => Monoid a => Ord a =>  M.Map a (S.Set a) -> [(a, S.Set a)] -> [a] -> [a]
findPareto _ [] results = results
findPareto bindings available results
  | not $ isPareto (fst $ head available) results = findPareto bindings (tail available) results
  | otherwise = findParetoHelper (head available) (M.filterWithKey removeWorse bindings) (tail available) results where
    isPareto c currentPareto = all (\x -> not (x `isObviouslyBetterThan` c)) currentPareto
    removeWorse = \x _ -> not ((fst $ (head available)) `isObviouslyBetterThan` x)

paretoSet:: Paretian a => Coverage a => Monoid a => Ord a => [a] -> [a]
paretoSet tests = findPareto (M.fromList opts) opts  [] where
  opts = makeOptions . filterMuchWorse $ S.fromList tests
