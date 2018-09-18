module Algorithm.Evolutionary (
  Population,
  mkPopulation,
  getPopulation,
  solve
  ) where

import Data.Functor
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Data.List (sortOn)
import Data.List.Split (chunksOf)

import Algorithm.Evolutionary.Internals.Population


initialise :: Applicative m => m genotype -> Int -> m (Population genotype)
initialise generateIndividual populationSize =
  mkPopulation <$> replicateM populationSize generateIndividual


recombine :: Monad m => (ind -> ind -> m [a]) -> [ind] -> m [a]
recombine mateCouple parents =
  fmap concat . mapM (uncurry mateCouple) $ pairs (parents ++ reverse parents)
  where pairs = foldr makePair [] . chunksOf 2
        makePair [x,y] acc = (x, y) : acc
        makePair _ acc = acc


mutate :: (MonadRandom m, Traversable t) => (ind -> m ind) -> t ind -> m (t ind)
mutate = traverse


nextGeneration :: (MonadRandom m)
               => (Population ind -> m [ind])
               -> (ind -> ind -> m [ind])
               -> (ind -> m ind)
               -> (Population ind -> m (Population ind))
               -> Population ind
               -> m (Population ind)
nextGeneration selectParents mateCouple mutateIndividual selectSurvivors pop = do
  parents <- selectParents pop
  offspring <- mutate mutateIndividual =<< recombine mateCouple parents
  let newPopulation = mkPopulation (getPopulation pop ++ offspring)
  selectSurvivors newPopulation


solve :: (MonadRandom m, MonadIO m, Ord n, Num n, Show n)
      => m ind
      -> (ind -> n)
      -> (Population ind -> m [ind])
      -> (ind -> ind -> m [ind])
      -> (ind -> m ind)
      -> (Population ind -> m (Population ind))
      -> Int
      -> Int
      -> (Population ind -> Bool)
      -> m ind
solve generateIndividual fitness selectParents mateCouple mutateIndividual selectSurvivors populationSize maxIters finished =
  initialise generateIndividual populationSize >>= fmap (minimumOn fitness . getPopulation) . go 1 1
  where go gen iter pop
          | finished pop =
              liftIO $ putStrLn (unlines ["Finished", "Population: " ++ show gen, "Generation: " ++ show iter]) $> pop
          | iter >= maxIters =
              liftIO (putStrLn "Trying a new population") *> initialise generateIndividual populationSize >>= go (gen + 1) 1
          | otherwise =
              when (iter `rem` 500 == 0)
              (liftIO . putStrLn $ unlines [
                  "Generation: " ++ show iter,
                  "Best result: " ++ show (minimum . fmap fitness $ getPopulation pop)])
              *> nextGeneration selectParents mateCouple mutateIndividual selectSurvivors pop
              >>= go gen (iter + 1)

        minimumOn :: Ord b => (a -> b) -> [a] -> a
        minimumOn f = head . sortOn f