module Algorithm.Evolutionary.Operators.Selection (
  shuffleAndSelect
  ) where

import Control.Monad.Random.Class
import System.Random.Shuffle (shuffleM)
import Data.List (sortOn)

import Algorithm.Evolutionary.Internals.Population

shuffleAndSelect :: (MonadRandom m, Ord n, Num n) => Int -> (ind -> n) -> Population ind -> m [ind]
shuffleAndSelect numParents fitnessFunction =
  fmap (take numParents . sortOn fitnessFunction . take (2 * numParents)) . shuffleM . getPopulation
