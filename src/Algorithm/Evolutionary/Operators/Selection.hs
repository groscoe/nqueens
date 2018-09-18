module Algorithm.Evolutionary.Operators.Selection (
  shuffleAndSelect,
  selectNFittest
  ) where

import Control.Monad.Random.Class
import System.Random.Shuffle (shuffleM)
import Data.List (sortOn)

import Algorithm.Evolutionary.Internals.Population


shuffleAndSelect :: (MonadRandom m, Ord n, Num n) => (ind -> n) -> Int -> Population ind -> m [ind]
shuffleAndSelect fitnessFunction numParents  =
  fmap (take numParents . sortOn fitnessFunction . take (2 * numParents)) . shuffleM . getPopulation


selectNFittest :: (Applicative m, Ord n) => (ind -> n) -> Int -> Population ind -> m (Population ind)
selectNFittest fitnessFunction numSurvivors =
  pure . mkPopulation . take numSurvivors . sortOn fitnessFunction . getPopulation
