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

module DynamicSolving (

) where

import Gradeable

bestDynamicSolutions xs fs = sort $ solveDynamically xs fs

solveDynamically:: Gradeable b => [[a]] -> [(a -> b)]  -> [[a]]
solveDynamically xs fs = foldr mutator (zip xs fs) [] where
  mutator acc (x, f) = f acc x

-- to wszystko to będzie jeden foldr, tylko trzeba wymyślić sensownie jaki
