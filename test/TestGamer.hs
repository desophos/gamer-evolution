{-# LANGUAGE TemplateHaskell #-}
module TestGamer where

import           Gamer
import           Test.Invariant
import           Test.QuickCheck.All





return []
runTests :: IO Bool
runTests = $quickCheckAll
