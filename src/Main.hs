{-# OPTIONS_GHC -F -pgmF htfpp #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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
module Main (
    main
) where

import DynamicSolving
import Test.Framework
import Data.Monoid
import Test.QuickCheck
import Data.List

instance (Arbitrary a, Arbitrary b) => Arbitrary (Graded a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (Gr x y)

instance Show (a -> b) where
    show x = "Function"

prop_memptyIsGradeableIdentity grd = mempty `mappend` grd == grd `mappend` mempty &&
  mempty `mappend` grd == grd
    where types = grd:: Graded [Int] Int

prop_mappendIsGradableAssociativeOperator grd grd2 grd3 =
  (grd `mappend` grd2) `mappend` grd3 == grd `mappend` (grd2 `mappend` grd3)
    where types = (grd:: Graded [Int] Int, grd2 ::Graded [Int] Int, grd3:: Graded [Int] Int)

prop_gradeableIsProperlyOrdered grd grd2 grd3 = not $ grd > grd2 && grd2 > grd3 && grd3 > grd
  where types = (grd:: Graded [Int] Int, grd2 ::Graded [Int] Int, grd3:: Graded [Int] Int)

prop_bestSolutionIsFirstElementOfOrderedList elems = null elems ||
  (gr $ bestSolution (\x -> length x) elems) == (length $ head $ sortBy (\x y -> if (length x) < (length y) then GT else LT) elems)
    where types = elems::[[Int]]

prop_twoStepsDynamicSolvingBehavesAsReferenceImplementation grader mutator options = null options || length options > 5 ||
  twoStepsBest grader mutator options == referenceTwoStepsBest grader mutator options
    where types = (options::[[Int]], grader::[Int] -> Int, mutator:: [Int] -> [Int] -> [Int])



prn = solveDynamically maximum (\_ y -> if length y == 0 then [] else tail y) [[1,22],[1,2,3],[42,1]] 2
--main = htfMain htf_Main_thisModulesTests
main = putStrLn $ show prn
