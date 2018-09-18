module Algorithm.Evolutionary.Operators.Selection (
  shuffleAndSelect,
  selectFittest
  ) where

import Control.Monad.Random.Class
import System.Random.Shuffle (shuffleM)
import Data.List (sortOn)

import Algorithm.Evolutionary.Internals.Population

shuffleAndSelect :: (MonadRandom m, Ord n, Num n) => (ind -> n) -> Int -> Population ind -> m [ind]
shuffleAndSelect fitnessFunction numParents =
  fmap (take numParents . sortOn fitnessFunction . take (2 * numParents)) . shuffleM . getPopulation

selectFittest :: Applicative m => Ord n => (ind -> n) -> Int -> Population ind -> m [ind]
selectFittest fitness numSurvivors =
  pure . take numSurvivors . sortOn fitness . getPopulation
