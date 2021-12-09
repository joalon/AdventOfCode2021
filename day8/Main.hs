module Main where

import System.Environment


main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let list = lines content

	putStrLn $ "Part 1: " ++ show (part1 list)


part1 list = foldr isSegmentOutput 0 outputs
	where
	    outputs = concat (map (\x -> words $ drop 2 $ dropWhile (/= '|') x) list)
	    isSegmentOutput str acc = case length str of
								2 -> acc + 1
								3 -> acc + 1
								4 -> acc + 1
								7 -> acc + 1
								_ -> acc


