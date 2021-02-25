{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module TestGamer where

import           Data.List                      ( stripPrefix )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Vector                   as V
import           Evolution                      ( Agent(..)
                                                , EvolutionParams(..)
                                                , evolve
                                                , getFitness
                                                )
import           GHC.IO.Handle                  ( hDuplicate
                                                , hDuplicateTo
                                                )
import           Gamer                          ( GamerParams(..)
                                                , dilemma
                                                , newPlayers
                                                , playGame
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
                                                , counterexample
                                                , cover
                                                , quickCheckAll
                                                , verboseCheck
                                                , withMaxSuccess
                                                )
import           Text.Regex.TDFA                ( (=~) )
import           Util                           ( (<<)
                                                , combineWith
                                                , matchups2
                                                )


prop_evolveFitness :: GamerParams -> EvolutionParams -> Gen Property
prop_evolveFitness gParams eParams@EvolutionParams {..} = do
    let game = playGame dilemma 2
        avgFit =
            combineWith (/)
                . map (realToFrac .)
                $ [sum . map agentFitness . getFitness game . matchups2, length]
    pop  <- newPlayers gParams { gamerActions = 2 } evolvePopSize
    pop' <- evolve eParams game matchups2 pop
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
    verboseCheck prop_evolveFitness
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

    return True << mapM_
        (hPutStrLn stdout')
        ("correlations:" : [ k ++ " = " ++ show v | (k, v) <- Map.assocs rs ])


return []
runTests :: IO Bool
runTests = $quickCheckAll
