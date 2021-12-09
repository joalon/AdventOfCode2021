module Lib
    ( part1
    , part2
    , slidingWindow
    , compareLast
    ) where


part1 list = compareLast 0 list


part2 mylist = compareLast 0 (map sum $ slidingWindow mylist 3 [])


compareLast num (x:[]) = num
compareLast num (x:xs) = case x < head xs of
	True -> compareLast (num+1) xs
	False -> compareLast num xs


slidingWindow list num acc = case length list >= num of
	True -> slidingWindow (drop 1 list) num (acc ++ [take num list])
	False -> acc

