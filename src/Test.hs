-----------------------------------------------------------------------------
--
-- Module      :  Test
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

module Test (

) where

import Data.Monoid
import Data.Range.Range


class (Ord a) => Test a where
  coverage:: a -> [Range Int]
  isCombinationUseful:: a -> a -> Bool

type Id = Int
type Time = Sum Double
type Value = Sum Double

data TestData = TD {ids::[Id],
  time::Time,
  value::Value,
  ranges::[Range Int],
  worthMerging::(TestData -> TestData -> Bool),
  comp::(TestData -> TestData -> Ordering)}

instance Eq TestData where
  a == b = ids a == ids b

instance Ord TestData where
  compare a b = (comp a) a b

instance Test TestData where
    coverage = ranges
    isCombinationUseful a b = (worthMerging a) a b

instance Monoid TestData where
    mempty = TD [] (Sum 0) (Sum 0) [] (\_ _ -> True) (\_ _ -> LT)
    a `mappend` b = TD (ids a <> ids b) (time a <> time b) (value a <> value b)
      (ranges a `union` ranges b) (worthMerging a) (comp a)
