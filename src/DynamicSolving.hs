-----------------------------------------------------------------------------
--
-- Module      :  DynamicSolving
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

module DynamicSolving (
  htf_thisModulesTests
) where

import Test.Framework

--bestDynamicSolutions xs fs = sort $ solveDynamically xs fs

--solveDynamically:: Gradeables b => [[a]] -> [(a -> b)]  -> [[a]]
--solveDynamically xs fs = foldr mutator (zip xs fs) [] where
--  mutator acc (x, f) = f acc x


-- to wszystko to będzie jeden foldr, tylko trzeba wymyślić sensownie jaki
