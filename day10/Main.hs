
module Main where

import System.Environment
import Data.List

data State a = Complete | Incomplete | Incorrect a deriving (Show, Eq)


main :: IO ()
main = do
	args <- getArgs
	content <- readFile (args !! 0)
	let list = lines content

	putStrLn $ "Part 1: " ++ show (part1 list)


part1 list = incorrectParens * 3 + incorrectSquares * 57 + incorrectCurlies * 1197 + incorrectArrows * 25137
	where
        incorrectParens = length $ filter (\x -> (validate x) == Incorrect ')') list
        incorrectSquares = length $ filter (\x -> (validate x) == Incorrect ']') list
        incorrectCurlies = length $ filter (\x -> (validate x) == Incorrect '}') list
        incorrectArrows = length $ filter (\x -> (validate x) == Incorrect '>') list


validate list = validate' list []

validate' [] [] = Complete
validate' [] nonempty = Incomplete
validate' (x:xs) stack
	| x == '[' || x == '{' || x == '<' || x == '(' =
		validate' xs (x:stack)
validate' (x:xs) (y:ys)
	| y /= (complementParen x) = Incorrect x
validate' (x:xs) (y:ys) = validate' xs ys
		

complementParen ']' = '['
complementParen '}' = '{'
complementParen '>' = '<'
complementParen ')' = '('

complementParen '[' = ']'
complementParen '{' = '}'
complementParen '<' = '>'
complementParen '(' = ')'



part2 list = (sort autocompleteScores) !! ((length autocompleteScores) `div` 2)
    where 
        autocompleteScores = map scoreCompletion $ map getCompletions $ filter (\x -> (validate x) == Incomplete) list


scoreCompletion = foldl accumulator 0
    where 
        accumulator acc ')' = (acc * 5) + 1
        accumulator acc ']' = (acc * 5) + 2
        accumulator acc '}' = (acc * 5) + 3
        accumulator acc '>' = (acc * 5) + 4
        accumulator acc _ = error "Shouldn't happen"


getCompletions x = getCompletions' x []

getCompletions' [] nonempty = nonempty
getCompletions' (x:xs) stack
	| x == '[' || x == '{' || x == '<' || x == '(' =
		getCompletions' xs ((complementParen x):stack)
getCompletions' (x:xs) (y:ys) = getCompletions' xs ys


