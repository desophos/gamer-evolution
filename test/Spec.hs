module Main where

import qualified TestUtil as U
import qualified TestEvolution as E
import System.Exit

main :: IO ()
main = do
    good <- and <$> sequence [U.runTests, E.runTests]
    if good
        then exitSuccess
        else exitFailure
