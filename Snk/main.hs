module Main where

import Grid

main = do
  mapM_ putStrLn $ showGrid exampleGrid