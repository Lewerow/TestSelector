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
  grade,
  bestSolution,
  twoStepsBest,
  referenceTwoStepsBest,
  solveDynamically,
  Graded(Gr), val, gr
) where

import Data.Monoid
import qualified Data.Foldable as F (maximum, Foldable)
import Control.Monad (liftM2)

data Graded a b = Gr {val:: a, gr:: b} deriving (Show)

instance Eq b => Eq (Graded a b) where
  (Gr _ x) == (Gr _ y) = x == y

instance Ord b => Ord (Graded a b) where
  (Gr _ x) <= (Gr _ y) = x <= y

instance (Monoid a, Num b) => Monoid (Graded a b) where
  mempty = Gr mempty 0
  (Gr a b) `mappend` (Gr c d) = Gr (a `mappend` c) (b+d)

grade :: (a -> b) -> a -> Graded a b
grade f v = Gr v $ f v

gradeAll :: Functor f => (a -> b) -> f a -> f (Graded a b)
gradeAll grader opts = fmap (\opt -> Gr opt $ grader opt) opts

bestSolution :: Monoid a => Ord b => Num b => (a -> b) -> [a] -> Graded a b
bestSolution _ [] = mempty
bestSolution grader opts = maximum $ gradeAll grader opts

takeStep:: Monoid a => Ord b => Num b => Functor f => F.Foldable f =>
  (a -> b) -> (a -> a -> a) -> f (Graded a b) -> f a -> f (Graded a b)
takeStep grader mutator state opts = let rate = grade grader in
  fmap (\x -> mappend x $ F.maximum $ fmap (rate . (mutator $ val x)) opts) state

solveDynamically:: Monoid a => Ord b => Num b => Integral c => Functor f => F.Foldable f => Monoid (f (Graded a b))
  => (a -> b) -> (a -> a -> a) -> f a -> c -> Graded a b
solveDynamically grader mutator options num = solveDynamicallyHelper grader mutator options num (gradeAll grader options) where
  solveDynamicallyHelper _ _ _ 0 _ = mempty
  solveDynamicallyHelper _ _ _ 1 s = F.maximum s
  solveDynamicallyHelper g m o n s = solveDynamicallyHelper g m o (n-1) newState where
    newState = takeStep g m s o

twoStepsBest g u o = solveDynamically g u o 1
{-

twoStepsBest:: Monoid a => Ord b => Num b => (a -> b) -> (a -> a -> a) -> [a] -> Graded a b
twoStepsBest grader updater opts = let rate = grade grader in
    maximum $ takeStep grader updater (map rate opts) opts
-}
referenceTwoStepsBest:: Monoid a => Ord b => Num b => (a -> b) -> (a -> a -> a) -> [a] -> Graded a b
referenceTwoStepsBest grader updater opts = let lastStep = gradeAll grader opts in
  let stateSpace = map (\x -> map (updater $ val x) opts) lastStep in
    let gradedOpts = map (gradeAll grader) stateSpace in
      let bestOptions = map maximum gradedOpts in
        maximum $ zipWith mappend lastStep bestOptions
