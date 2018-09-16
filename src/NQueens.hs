module NQueens (solveNQueens, showBoard) where

import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Data.List (transpose, intercalate)
import System.Random.Shuffle

import GeneralEvolutionary


type Board = [Int]


generateBoard :: MonadRandom m => Int -> m Board
generateBoard n = shuffleM [1..n]


fitness :: Int -> Board -> Int
fitness n (column:columns) = numChecks column columns + fitness n columns
  where numChecks y ys =
          length . filter id $ zipWith (==) [y+1..n] ys ++ zipWith (==) [y-1, y-2..1] ys
fitness _ _ = 0


mutate :: MonadRandom m => Int -> Double -> Board -> m Board
mutate n mutationProbability individual = do
  p <- getRandomR (0, 1.0)
  [i, j] <- take 2 <$> getRandomRs (1, n)
  pure $ if p <= mutationProbability
         then individual
         else swap i j individual


mateCouple :: MonadRandom m => Board -> Board -> m [Board]
mateCouple parent1 parent2 = do
  crossoverPoint <- getRandomR (1, length parent1)
  let (child1, child2) = cutAndCrossFill crossoverPoint parent1 parent2
  pure $ [child1, child2]


solveNQueens :: (MonadIO m, MonadRandom m) => Int -> Int -> m Board
solveNQueens populationSize n =
  let numParents = 2
      maxIters = 10000
      mutationProbability = 0.8
  in
    solve (generateBoard n) (fitness n) numParents mateCouple (mutate n mutationProbability) populationSize maxIters


-- Auxiliary functions
cutAndCrossFill :: Int -> Board -> Board -> (Board, Board)
cutAndCrossFill n i1 i2 =
  let m = length i1 - n
      l1 = take n i1
      l2 = take n i2
      r1 = take m $ filter (not . (`elem` l1)) i2
      r2 = take m $ filter (not . (`elem` l2)) i1
  in (l1 ++ r1, l2 ++ r2)


swap :: Int -> Int -> [a] -> [a]
swap i j = swap' (i-1) (j-1)
  where swap' f s xs =
          zipWith (\x y -> if x == f then xs !! s
                          else if x == s then xs !! f
                          else y) [0..] xs

showBoard :: Int -> Board -> String
showBoard n = unlines . map (intercalate "|") . transpose . getBoardMatrix n
  where getBoardMatrix n =
          foldr (\column -> (:) [if x == column then "Q" else "_" | x <- [1..n]]) []
