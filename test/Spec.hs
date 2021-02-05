module Main where

import qualified TestUtil as U
import System.Exit

main :: IO ()
main = do
    good <- and <$> sequence [U.runTests]
    if good
        then exitSuccess
        else exitFailure
