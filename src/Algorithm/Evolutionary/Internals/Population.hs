module Algorithm.Evolutionary.Internals.Population (
  Population,
  mkPopulation,
  getPopulation
  ) where


newtype Population genotype = Population { getPopulation :: [genotype] }
  deriving (Ord, Eq, Show)


mkPopulation :: [ind] -> Population ind
mkPopulation = Population
