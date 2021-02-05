{-# LANGUAGE TemplateHaskell #-}
module TestEvolution where

import Test.QuickCheck.All
import Evolution
import Util
import Instances()


prop_mergeAgentsUnique :: [Agent a] -> Bool
prop_mergeAgentsUnique = unique . mergeAgents

prop_mergeAgentsKeepAll :: [Agent a] -> Bool
prop_mergeAgentsKeepAll xs = all (`elem` mergeAgents xs) xs

prop_mergeAgentsNoNew :: [Agent a] -> Bool
prop_mergeAgentsNoNew xs = all (`elem` xs) (mergeAgents xs)


return []
runTests :: IO Bool
runTests = $quickCheckAll
