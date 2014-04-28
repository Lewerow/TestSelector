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
solveDynamically x f = solveDynamicallyHelper x f [] where
  solveDynamicallyHelper [] _ r = r
  solveDynamicallyHelper _ [] _ = undefined

-- to wszystko to będzie jeden foldr, tylko trzeba wymyślić sensownie jaki
