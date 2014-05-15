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
import qualified Data.Set as S
import Data.Monoid

main = putStrLn . show $ calculate (S.fromList sums) (\x y -> x > y) where -- htfMain htf_importedTests
  sums = take 12 produce where
    produce = map Sum [1..]
