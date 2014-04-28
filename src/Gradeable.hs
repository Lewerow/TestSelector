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

module Gradeable (
  Gradeable
) where

class Gradeable a where
  grade :: a -> Double
