module GeneralEvolutionary (Population (..), solve) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Data.List (sortOn)
import Data.List.Split (chunksOf)
import System.Random.Shuffle (shuffleM)


newtype Population genotype = Population { getPopulation :: [genotype] }
  deriving (Ord, Eq, Show)


initialise :: (Enum n, Num n, Applicative m) => m genotype -> n -> m (Population genotype)
initialise generateIndividual populationSize =
  Population <$> traverse (const generateIndividual) [1..populationSize]


selectParents :: (MonadRandom m, Ord n) => Int -> (a -> n) -> Population a -> m [a]
selectParents numParents fitness (Population p) =
  take numParents . sortOn fitness . take (numParents * 2) <$> shuffleM p


recombine :: Monad m => (ind -> ind -> m [a]) -> [ind] -> m [a]
recombine mateCouple parents =
  fmap concat . mapM (uncurry mateCouple) $ pairs (parents ++ reverse parents)
  where pairs = foldr makePair [] . chunksOf 2
        makePair [x,y] acc = (x, y) : acc
        makePair _ acc = acc


mutate :: (MonadRandom m, Traversable t) => (ind -> m ind) -> t ind -> m (t ind)
mutate = traverse


selectNextGen :: (Num n, Ord n, Applicative m) => (ind -> n) -> Int -> Population ind -> m (Population ind)
selectNextGen fitness numOffspring (Population p) =
  pure . Population . drop numOffspring . sortOn (negate . fitness) $ p


nextGeneration :: (MonadRandom m, Ord n, Num n)
               => (ind -> n)
               -> Int
               -> (ind -> ind -> m [ind])
               -> (ind -> m ind)
               -> Population ind
               -> m (Population ind)
nextGeneration fitness numParents mateCouple mutateIndividual population = do
  parents <- selectParents numParents fitness population
  offspring <- recombine mateCouple parents >>= mutate mutateIndividual
  let newPopulation = Population (getPopulation population ++ offspring)
  selectNextGen fitness (length offspring) newPopulation


finished :: (Eq n, Num n) => (ind -> n) -> Population ind -> Bool
finished fitness (Population p) = any ((== 0) . fitness) p


minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = head . sortOn f


solve :: (MonadRandom m, MonadIO m, Ord n, Num n, Show n)
      => m ind
      -> (ind -> n)
      -> Int
      -> (ind -> ind -> m [ind])
      -> (ind -> m ind)
      -> Int
      -> Int
      -> m ind
solve generateIndividual fitness numParents mateCouple mutateIndividual populationSize maxIters =
  initialise generateIndividual populationSize >>= fmap (minimumOn fitness . getPopulation) . go 1 1
    where go gen iter pop
            | finished fitness pop =
              liftIO $ putStrLn (unlines ["Finished", "Population: " ++ show gen, "Generation: " ++ show iter]) *> pure pop
            | iter >= maxIters =
              liftIO (putStrLn "Trying a new population") *> initialise generateIndividual populationSize >>= go (gen + 1) 1
            | otherwise =
              when (iter `rem` 500 == 0)
              (liftIO . putStrLn $ unlines [
                  "Generation: " ++ show iter,
                  "Best result: " ++ show (minimum . fmap fitness $ getPopulation pop)])
              *> nextGeneration fitness numParents mateCouple mutateIndividual pop >>= go gen (iter + 1)
