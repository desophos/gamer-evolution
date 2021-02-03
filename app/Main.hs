module Main where

import Test.QuickCheck
import Gamer
import Evolution
import Util


-- reward matrix for Prisoner's Dilemma
-- 0 = cooperate; 1 = defect
dilemma :: [Int] -> [Int]
dilemma [0,0] = [3,3]
dilemma [1,0] = [5,0]
dilemma [0,1] = [0,5]
dilemma [1,1] = [1,1]


main :: IO ()
main =
    let players = map agentChromosome <$> newPlayers 20
        runMatch = playGame dilemma 3
        matches = fmap matchups2 players
        results = map runMatch <$> matches
    in print =<< generate results
