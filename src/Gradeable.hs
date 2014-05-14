

-----------------------------------------------------------------------------
--
-- Module      :  Gradeable
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

module Gradeable (
  Gradeables,
  htf_thisModulesTests
) where

import Test.Framework

class Gradeables a where
  grade :: a -> Double
