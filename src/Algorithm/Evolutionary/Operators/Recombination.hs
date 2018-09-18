module Algorithm.Evolutionary.Operators.Recombination (
  cutAndCrossFill
  ) where

cutAndCrossFill :: Eq a => Int -> [a] -> [a] -> ([a], [a])
cutAndCrossFill n i1 i2 =
  let m = length i1 - n
      l1 = take n i1
      l2 = take n i2
      r1 = take m $ filter (not . (`elem` l1)) i2
      r2 = take m $ filter (not . (`elem` l2)) i1
  in (l1 ++ r1, l2 ++ r2)
