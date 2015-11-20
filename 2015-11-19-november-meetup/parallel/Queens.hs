module Main where

import Control.Parallel
import Control.Parallel.Strategies hiding (parMap)
import System.Environment (getArgs)

-- A queen has coordinates
data Queen = Queen Int Int

-- Check whether two queens threaten each other.
-- There is no need to check x==a since we never place two queens in
-- the same column.
conflict :: Queen -> Queen -> Bool
conflict (Queen x y) (Queen a b) = y==b || abs (x-a) == abs (y-b)

-- What are the possible places for a queen in column k?
-- n is size of the board, qs are queens already on the board.
nexts :: Int -> Int -> [Queen] -> [Queen]
nexts n k qs = filter ok [Queen k i | i <- [0..n-1]]
  where ok q = not $ any (conflict q) qs

-- In how many ways can we fill columns k..n-1 when queens qs are
-- already on the board?
-- Invariant: qs consists of one queen per each column 0..k-1
-- Invariant: qs don't conflict
count :: Int -> Int -> [Queen] -> Int
count n k qs
  | n == k    = 1
  | otherwise = sum (map f children `using` parList rseq)
  where f q = count n (k+1) (q:qs)
        children = nexts n k qs

-- In how many ways can we place n queens on a nxn chess board?
nqueens :: Int -> Int
nqueens n = count n 0 []

main = do
  [n'] <- getArgs
  let n = read n'
  print $ nqueens n



parMap f [] = []
parMap f (x:xs) = a `par` as `pseq` (a : as)
  where a = f x
        as = parMap f xs
