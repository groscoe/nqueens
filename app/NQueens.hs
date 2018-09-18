module NQueens (solveNQueens, showBoard) where

import Control.Monad.IO.Class
import Control.Monad.Random.Class
import Data.List (transpose, intercalate)
import System.Random.Shuffle

import Algorithm.Evolutionary.Operators.Selection (shuffleAndSelect)
import Algorithm.Evolutionary.Operators.Mutation (swapAlleles)
import Algorithm.Evolutionary.Operators.Recombination (cutAndCrossFill)
import Algorithm.Evolutionary


type Board = [Int]


generateBoard :: MonadRandom m => Int -> m Board
generateBoard n = shuffleM [1..n]


fitness :: Int -> Board -> Int
fitness n (column:columns) = numChecks column columns + fitness n columns
  where numChecks y ys =
          length . filter id $ zipWith (==) [y+1..n] ys ++ zipWith (==) [y-1, y-2..1] ys
fitness _ _ = 0


selectParents :: (MonadRandom m) => Int -> Population Board -> m [Board]
selectParents n = shuffleAndSelect 2 (fitness n)


mateCouple :: MonadRandom m => Board -> Board -> m [Board]
mateCouple parent1 parent2 = do
  crossoverPoint <- getRandomR (1, length parent1)
  let (child1, child2) = cutAndCrossFill crossoverPoint parent1 parent2
  pure [child1, child2]



finished :: Int -> Population Board -> Bool
finished n = any ((== 0) . fitness n) . getPopulation


solveNQueens :: (MonadIO m, MonadRandom m) => Int -> Int -> m Board
solveNQueens populationSize n =
  let maxIters = 10000
      mutationProbability = 0.8
  in
    solve (generateBoard n)
          (fitness n)
          (selectParents n)
          mateCouple
          (swapAlleles n mutationProbability)
          populationSize
          maxIters
          (finished n)


showBoard :: Int -> Board -> String
showBoard n = unlines . map (intercalate "|") . transpose . getBoardMatrix n
  where getBoardMatrix m =
          foldr (\column -> (:) [if x == column then "Q" else "_" | x <- [1..m]]) []
