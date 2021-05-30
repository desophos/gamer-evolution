{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.Set                      as S
import           Evolution                      ( Agent
                                                , EvolutionParams(..)
                                                , avgFitness
                                                , collectEvolve
                                                )
import           GHC.Float                      ( float2Int
                                                , int2Float
                                                )
import           Gamer                          ( GamerParams(..)
                                                , dilemma
                                                , genPlayers
                                                , playGame
                                                )
import           Graphics.EasyPlot              ( Graph2D(Function2D)
                                                , Option(Title)
                                                , Option2D(Range, Step)
                                                , Plot(plot)
                                                , TerminalType(PNG)
                                                )
import           Test.QuickCheck                ( generate )
import           Util                           ( matchups )


main :: IO Bool
main = graphFitness
    =<< generate (collectEvolve eParams game matchup =<< players)
  where
    gParams = GamerParams { .. }
      where
        gamerActions = 2
        gamerStates  = 8
        gamerMemory  = 3
    eParams = EvolutionParams { .. }
      where
        evolveGenerations = 50
        evolvePopSize     = 50
        evolveSurvivors   = 30
        evolveMutateP     = 0.01
    game    = playGame dilemma 50
    players = genPlayers gParams 50
    matchup = matchups 2 . S.fromDistinctAscList


graphFitness :: [[Agent a]] -> IO Bool
graphFitness run = plot (PNG "plot.png") $ Function2D
    [Title "Average Fitness"]
    [Range 0 (int2Float $ length avgRun - 1), Step 1]
    ((avgRun !!) . float2Int)
    where avgRun = map avgFitness run
