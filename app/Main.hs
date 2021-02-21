{-# LANGUAGE RecordWildCards #-}
module Main where

import           Evolution
import           Gamer
import           Test.QuickCheck
import           Util


main :: IO ()
main =
    let params = GamerParams { .. }
              where
                gamerActions = 2
                gamerStates  = 8
                gamerMemory  = 1
        players  = map agentChromosome <$> newPlayers params 20
        runMatch = playGame dilemma 3
        matches  = fmap matchups2 players
        results  = map runMatch <$> matches
    in  print =<< generate results
