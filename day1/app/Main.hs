module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let list = map read (lines content) :: [Int]

  putStrLn $ "Part 1: " ++ show (part1 list)
  putStrLn $ "Part 2: " ++ show (part2 list)

