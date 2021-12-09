module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  content <- readFile (args !! 0)
  let list = map read (lines content) :: [Int]

  putStrLn $ "Part 1: " ++ show (part1 list)
  putStrLn $ "Part 2: " ++ show (part2 list)

part1 list = compareLast 0 list


part2 mylist = compareLast 0 (map sum $ slidingWindow mylist 3 [])


compareLast num (x:[]) = num
compareLast num (x:xs) = case x < head xs of
	True -> compareLast (num+1) xs
	False -> compareLast num xs


slidingWindow list num acc = case length list >= num of
	True -> slidingWindow (drop 1 list) num (acc ++ [take num list])
	False -> acc

