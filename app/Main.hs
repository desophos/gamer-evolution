module Main where

import Data.List
import Test.QuickCheck
import Lib

-- reward matrix for Prisoner's Dilemma
-- 0 = cooperate; 1 = defect
dilemma :: [Int] -> [Int]
dilemma [0,0] = [3,3]
dilemma [1,0] = [5,0]
dilemma [0,1] = [0,5]
dilemma [1,1] = [1,1]

sameMatch :: (Eq a) => [a] -> [a] -> Bool
sameMatch xs ys = all (`elem` ys) xs

matchups2 :: (Eq a) => [a] -> [[a]]
matchups2 xs = nubBy sameMatch [[x,y] | x <- xs, y <- xs, x /= y]

main :: IO ()
main =
    let players = vectorOf 20 randomChromosome
        matches = fmap matchups2 players
        runMatch = playGame dilemma 3
        results = map runMatch <$> matches
    in print =<< generate results
