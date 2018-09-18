module Algorithm.Evolutionary.Operators.Selection (
  shuffleAndSelect,
  dropWeakest
  ) where

import Control.Monad.Random.Class
import System.Random.Shuffle (shuffleM)
import Data.List (sortOn)

import Algorithm.Evolutionary.Internals.Population

shuffleAndSelect :: (MonadRandom m, Real n) => (ind -> n) -> Int -> Population ind -> m [ind]
shuffleAndSelect fitnessFunction numParents =
  fmap (take numParents . sortOn fitnessFunction . take (2 * numParents)) . shuffleM . getPopulation

dropWeakest :: (Applicative m, Real n) => (ind -> n) -> Int -> Population ind -> m [ind]
dropWeakest fitness numToDrop =
  pure . drop numToDrop . sortOn (negate . fitness) . getPopulation
