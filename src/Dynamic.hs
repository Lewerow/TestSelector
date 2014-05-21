-----------------------------------------------------------------------------
--
-- Module      :  Dynamic
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

module Dynamic (
  calculate
) where

import Prelude hiding(lookup)
import Data.List hiding(lookup)
import qualified Data.Set as S
import Data.Map (Map, toList, fromSet, lookup)
import Data.Maybe (fromJust)
import Data.Monoid

calculate:: Monoid a => Ord a => S.Set a -> (a -> a -> Bool) -> [a]
calculate set f = map fst $ dynamic matches f (toList matches) [] where
  matches = prefilter set f

prefilter:: S.Set a -> (a -> a -> Bool) -> Map a (S.Set a)
prefilter set f = fromSet selector set where
  selector k = S.filter (f k) set

dynamic:: Monoid a => Ord a =>
  Map a (S.Set a) -> (a -> a -> Bool) -> [(a, S.Set a)] -> [(a, S.Set a)] -> [(a, S.Set a)]
dynamic matches f remaining ready
  | null remaining = ready
  | otherwise = dynamic matches f (fst filtered) (remaining ++ snd filtered ++ ready) where
    filtered = partition (not . S.null . snd) processed where
      processed = concatMap (step (\k -> fromJust $ lookup k matches) f) remaining where


step:: Monoid a => Ord a => (a -> S.Set a) -> (a -> a -> Bool) -> (a, S.Set a) -> [(a, S.Set a)]
step match f record = map choosePossible $ S.toList $ snd record where
  choosePossible x = (w, S.filter (f w) $ snd record `S.intersection` match x) where
    w = x <> fst record
