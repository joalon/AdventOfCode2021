module Lib
    ( part1
    , part2
    ) where


import Data.List


part1 list = depth * length
	where
		(depth, length) = calculatePosition (0, 0) list


calculatePosition pos [] = pos
calculatePosition (depth, length) (x:xs)
	| isPrefixOf "forward" x = calculatePosition (depth, length + x') xs
	| isPrefixOf "down" x = calculatePosition (depth + x', length ) xs
	| isPrefixOf "up" x = calculatePosition (depth - x', length ) xs
	where
		x' = (read ((words x) !! 1) :: Integer)


part2 list = depth * length
    where 
		(depth, length, _) = calculatePositionWithAim (0,0,0) list

				
calculatePositionWithAim pos [] = pos
calculatePositionWithAim (depth, length, aim) (x:xs)
	| isPrefixOf "forward" x = calculatePositionWithAim (depth + aim * x', length + x', aim) xs
	| isPrefixOf "down" x = calculatePositionWithAim (depth, length, (aim + x')) xs
	| isPrefixOf "up" x = calculatePositionWithAim (depth, length, aim - x') xs
	where
		x' = (read ((words x) !! 1) :: Integer)

