module Main where

import Control.Parallel
import System.Environment

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fa n = fib n + fib (n+1)

fb n = a `par` (b `pseq` (a+b))
  where a = fib n
        b = fib (n+1)

main = do
  [a,n'] <- getArgs
  let n = read n'
  print $ case a of "a" -> fa n
                    "b" -> fb n
