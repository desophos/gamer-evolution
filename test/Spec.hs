module Main where

import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import qualified TestEvolution                 as E
import qualified TestGamer                     as G
import qualified TestUtil                      as U

main :: IO ()
main = do
    good <- and <$> sequence [U.runTests, E.runTests, G.runTests, G.analyzeEvolveFitness]
    if good then exitSuccess else exitFailure
