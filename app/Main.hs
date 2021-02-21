module Main where

import           Evolution
import           Gamer
import           Test.QuickCheck
import           Util


main :: IO ()
main =
    let players  = map agentChromosome <$> newPlayers 20
        runMatch = playGame dilemma 3
        matches  = fmap matchups2 players
        results  = map runMatch <$> matches
    in  print =<< generate results
