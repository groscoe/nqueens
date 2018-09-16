module Main where

import System.Environment (getArgs)
import Text.Read (readMaybe)

import NQueens

main :: IO ()
main = do
  args <- getArgs
  let n = getNFromArgs args
      populationSize = 20
  putStrLn $ "Solving the N-Queens problem with n = " ++ show n

  solution <- solveNQueens populationSize n
  putStrLn $ "Solution: " ++ show solution

  putStrLn $ showBoard n solution

  where defaultN = 50
        getNFromArgs [] = defaultN -- default value
        getNFromArgs (arg:_) = case readMaybe arg of
          Nothing -> defaultN
          Just m -> m
