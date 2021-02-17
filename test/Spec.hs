module Spec where

import           System.Exit                    ( exitFailure
                                                , exitSuccess
                                                )
import qualified TestEvolution                 as E
import qualified TestUtil                      as U

main :: IO ()
main = do
    good <- and <$> sequence [U.runTests, E.runTests]
    if good then exitSuccess else exitFailure
