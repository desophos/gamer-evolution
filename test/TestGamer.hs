{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module TestGamer where

import           Data.List                      ( foldl1'
                                                , stripPrefix
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Ord                       ( Down(Down) )
import qualified Data.Set                      as S
import qualified Data.Vector                   as V
import           Evolution                      ( Agent(..)
                                                , EvolutionParams(..)
                                                , collectEvolve
                                                , evolve
                                                , getFitness
                                                )
import           GHC.Float                      ( float2Int
                                                , int2Float
                                                )
import           GHC.IO.Handle                  ( hDuplicate
                                                , hDuplicateTo
                                                )
import           Gamer                          ( GamerParams(..)
                                                , PlayerState
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
import           Statistics.Sample              ( correlation )
import           System.IO                      ( IOMode(WriteMode)
                                                , hClose
                                                , hPutStrLn
                                                , openFile
                                                , stdout
                                                )
import           Test.QuickCheck                ( Gen
                                                , Property
                                                , arbitrary
                                                , counterexample
                                                , cover
                                                , generate
                                                , quickCheckAll
                                                , vectorOf
                                                , verboseCheck
                                                , withMaxSuccess
                                                )
import           Text.Regex.TDFA                ( (=~) )
import           Util                           ( combineWith
                                                , matchWithBest
                                                , matchups
                                                )


matchup :: [Agent a] -> [[Agent a]]
--matchup = matchWithBest 2 10 (Down . agentFitness) . S.fromDistinctAscList
matchup = matchups 2 . S.fromDistinctAscList

prop_evolveFitness :: GamerParams -> EvolutionParams -> Gen Property
prop_evolveFitness gParams eParams@EvolutionParams {..} = do
    let game   = playGame dilemma 100
        avgFit = combineWith
            (/)
            [ sum . map agentFitness . getFitness game . matchup
            , realToFrac . length
            ]
    pop  <- genPlayers gParams { gamerActions = 2 } evolvePopSize
    pop' <- evolve eParams game matchup pop
    let dFit = avgFit pop' - avgFit pop
    return
        . counterexample ("dFit = " ++ show dFit)
        . cover 90 (dFit > 0) "increased fitness"
        $ True -- we only care about the statistics


analyzeEvolveFitness :: IO Bool
analyzeEvolveFitness = do
    let filename = "prop_evolveFitness.log"
        regex    = "([a-zA-Z]+) = ([-0-9.]+)"

    stdout' <- hDuplicate stdout
    file    <- openFile filename WriteMode
    hDuplicateTo file stdout
    verboseCheck $ withMaxSuccess 500 prop_evolveFitness
    hClose file
    text <- readFile filename

    let allMatches = text =~ regex :: [[String]]
        allData    = Map.fromListWith
            (V.++)
            [ (stripGet $ match !! 1, V.singleton . read $ match !! 2)
            | match <- allMatches
            ]
            where stripGet s = fromMaybe s (stripPrefix "get" s)
        pairFit = Map.map $ V.zip (allData Map.! "dFit")
        rs      = Map.map correlation $ pairFit allData

    mapM_
        (hPutStrLn stdout')
        ("correlations:" : [ k ++ " = " ++ show v | (k, v) <- Map.assocs rs ])
    return True


graphEvolveFitness :: IO Bool
graphEvolveFitness = do
    runs <- generate $ vectorOf
        10
        (arbitrary >>= \x -> arbitrary >>= \y -> genCollectEvolve x y)
    let avgRun = mergeRuns $ map (map avgFit) runs
    plot (PNG "plot.png") $ Function2D
        [Title "Average Fitness"]
        [Range 0 (int2Float $ length avgRun - 1), Step 1]
        ((avgRun !!) . float2Int)
  where
    avgFit :: [Agent a] -> Float
    avgFit = combineWith (/) [sum . map agentFitness, realToFrac . length]
    mergeRuns :: [[Float]] -> [Float]
    mergeRuns = foldl1' (zipWith (\x y -> (x + y) / 2))
    game      = playGame dilemma 5
    genCollectEvolve
        :: GamerParams -> EvolutionParams -> Gen [[Agent [PlayerState]]]
    genCollectEvolve gParams eParams@EvolutionParams {..} = do
        pop <- genPlayers gParams { gamerActions = 2 } evolvePopSize
        collectEvolve eParams game matchup pop


return []
runTests :: IO Bool
runTests = $quickCheckAll
