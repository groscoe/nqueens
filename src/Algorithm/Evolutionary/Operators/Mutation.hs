module Algorithm.Evolutionary.Operators.Mutation (
  swapAlleles
  ) where

import Control.Monad.Random.Class

swapAlleles :: MonadRandom m => Int -> Double -> [a] -> m [a]
swapAlleles numGenes mutationProbability individual = do
  p <- getRandomR (0, 1.0)
  [i, j] <- take 2 <$> getRandomRs (1, numGenes)
  pure $ if p <= mutationProbability
         then individual
         else swap i j individual

swap :: Int -> Int -> [a] -> [a]
swap i j = swap' (i-1) (j-1)
  where swap' f s xs =
          zipWith (\x y -> if x == f then xs !! s
                          else if x == s then xs !! f
                          else y) [0..] xs
