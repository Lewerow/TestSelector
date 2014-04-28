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

solveDynamically:: Gradeable b => [[a]] -> [(a -> b)]  -> [[a]]
solveDynamically xs fs = foldr mutator zip(xs, fs) [] where
  mutator acc (x, f) = f acc x

-- to wszystko to będzie jeden foldr, tylko trzeba wymyślić sensownie jaki
