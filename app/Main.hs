module Main where

import Test.QuickCheck
import Lib

-- reward matrix for Prisoner's Dilemma
-- 0 = cooperate; 1 = defect
dilemma :: [Int] -> [Int]
dilemma [0,0] = [3,3]
dilemma [1,0] = [5,0]
dilemma [0,1] = [0,5]
dilemma [1,1] = [1,1]

main :: IO ()
main =
    let players = vectorOf 2 randomChromosome
    in print =<< generate (playGame dilemma 3 <$> players)
