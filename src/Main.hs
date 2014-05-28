{-# LANGUAGE CPP, TemplateHaskell #-}
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

{--import System.Environment(getArgs)
import Test.Framework
import Test.Framework.Tutorial
import {-@ HTF_TESTS @-} Gradeable
import DynamicSolving
import {-@ HTF_TESTS @-}  Clusterization
import {-@ HTF_TESTS @-}  Structuring
import {-@ HTF_TESTS @-}  Selection
--}
import Dynamic
import Data.Set (Set, fromList, toList, findMin)
import Data.Monoid
import Test
import Control.Monad
import System.IO
import Data.List(sortBy)
import Data.List.Split
import Data.Range.Range
import {-@ HTF_TESTS @-} ParetoSearch
import qualified Test.Framework as HTF

instance Show TestData where
    show x = "tests: " ++ (show $ ids x) ++
      " total time: " ++ (show . getSum $ time x) ++
      " additional value: " ++ (show . getSum $ value x) ++
      " covered ranges: " ++ (show $ ranges x) ++ "\r\n"

instance Coverage TestData where
  covers one two = (ranges one `intersection` ranges two) == ranges two
  uses a b = all ( `elem` (ids a)) $ ids b

instance Paretian TestData where
{-  a `isObviouslyBetterThan` b = if time a < time b && a `covers` b then GT
    else if time a > time b && b `covers` a then LT
      else EQ-}
  a `isObviouslyBetterThan` b = time a < time b && a `covers` b
  a `isWorthMergingWith` b = not (a `isObviouslyBetterThan` b) && maximum (ids a) < maximum (ids b)

convertToTests:: String -> [TestData]
convertToTests s = map (toTest) (lines s) where
  toTest:: String -> TestData
  toTest l = convert (splitOn ";" l) where
    convert:: [String] -> TestData
    convert x = TD ([rId]) (Sum rTm) (Sum rVal) (getCode rCode) mayMerge grade where
      rId = (read (x !! 0)) ::Int
      rTm = (read  (x !! 1)) ::Double
      rVal = (read (x !! 2)) ::Double
      rCode = (x !! 3) ::String

      getCode code = mergeRanges . map (\(x, y) -> SingletonRange y) . filter (\x -> fst x /= '0') $ zip code [1..]
      mayMerge one two = (head $ ids one) > (head $ ids two) && (together /= (ranges one) && together /= (ranges two)) where
          together = (ranges one) `union` (ranges two)

      grade a = (getSum $ time a) / (coveredPart^2)  where
        totalLines = fromIntegral $ length (x !! 3) ::Double
        coveredLines = fromIntegral . length . fromRanges $ ranges a ::Double
        coveredPart = coveredLines / totalLines ::Double

--main = HTF.htfMain htf_importedTests
--{-
main = do
  input <- readFile "D:\\Haskell\\TestSelector\\data.txt"
  let tests = convertToTests input
  let dyatt = calculate (fromList tests) (worthMerging $ findMin (fromList tests))
  let sorted = sortBy (\x y -> grade x x `compare` grade y y) dyatt
  writeFile "D:\\Haskell\\TestSelector\\results.txt" $ show sorted
  writeFile "D:\\Haskell\\TestSelector\\pareto.txt" $ show (sortBy (\x y -> grade x x `compare` grade y y) (paretoSet tests))
  putStrLn $ show $ take 10 sorted
--}
