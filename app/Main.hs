{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Set                      as S
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
        players  = map agentGenome <$> genPlayers params 20
        runMatch = playGame dilemma 3
        matches  = fmap (matchups 2 . S.fromDistinctAscList) players
        results  = map runMatch <$> matches
    in  print =<< generate results
