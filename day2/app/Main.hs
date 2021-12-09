module Main where

import Lib
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let list = lines content

	putStrLn $ "Part 1: " ++ show (part1 list)
	putStrLn $ "Part 2: " ++ show (part2 list)
