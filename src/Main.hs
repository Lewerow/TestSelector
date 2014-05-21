{-# LANGUAGE CPP, TemplateHaskell #-}
--{-# OPTIONS_GHC -F -pgmF htfpp #-}
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

instance Show TestData where
    show x = "tests: " ++ (show $ ids x) ++
      " total time: " ++ (show . getSum $ time x) ++
      " covered ranges: " ++ (show $ ranges x) ++ "\r\n"

convertToTests:: String -> [TestData]
convertToTests s = map (toTest) (lines s) where
  toTest:: String -> TestData
  toTest l = convert (splitOn ";" l) where
    convert:: [String] -> TestData
    convert x = TD ([rId]) (Sum rTm) (Sum rVal) (getCode rCode) mayMerge grade where
      rId = (read (x !! 0)) ::Int
      rTm = (read  (x !! 1)) ::Double;
      rVal = (read (x !! 2)) ::Double;
      rCode = (x !! 3) ::String

      getCode code = mergeRanges . map (\(x, y) -> SingletonRange y) . filter (\x -> fst x /= '0') $ zip code [1..]
      mayMerge one two = (maximum $ ids one) < (minimum $ ids two) && (together /= (ranges one) && together /= (ranges two)) where
          together = (ranges one) `union` (ranges two)

      grade a = (getSum $ time a) / (coveredPart^2)  where
        totalLines = fromIntegral $ length (x !! 3) ::Double
        coveredLines = fromIntegral . length . fromRanges $ ranges a ::Double
        coveredPart = coveredLines / totalLines ::Double

main = do
  input <- readFile "data.txt"
  let tests = convertToTests input
  let dyatt = calculate (fromList tests) (worthMerging $ findMin (fromList tests))
  let sorted = sortBy (\x y -> grade x x `compare` grade y y) dyatt
  writeFile "results.txt" $ show sorted
  putStrLn $ show $ take 10 sorted
