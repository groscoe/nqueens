module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import NQueens

data Parameters = Params {
  numberOfQueens :: Int,
  populationSize :: Int,
  maxIters :: Int
}

main :: IO ()
main = execParser opts >>= runNQueens
  where opts = info (params <**> helper)
          ( fullDesc
         <> header "nqueens - solve the n-queens problem with evolutionary programming." )

params :: Parser Parameters
params = Params
  <$> option auto
    ( short 'n'
   <> metavar "N"
   <> help "Number of queens in the board"
   <> showDefault
   <> value 50)
  <*> option auto
    ( short 'p'
   <> metavar "M"
   <> help "Population size"
   <> showDefault
   <> value 20)
  <*> option auto
    ( short 'i'
   <> metavar "K"
   <> help "Number of iterations before giving up"
   <> showDefault
   <> value 10000)

runNQueens :: Parameters -> IO ()
runNQueens (Params n popSize iters) = do
  putStrLn $ "Solving the N-Queens problem with n = " ++ show n
  solution <- solveNQueens popSize n iters
  putStrLn $ showBoard n solution
