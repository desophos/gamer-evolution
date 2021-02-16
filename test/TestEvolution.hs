{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module TestEvolution where

import Data.Typeable
import Test.QuickCheck
import Test.QuickCheck.All()
import Evolution
import Util
import Instances()


newtype ReproduceArgs a = ReproduceArgs (Double, [a] -> [Int], [Agent a]) deriving (Show)

instance (Show a, Typeable a, Eq a, Arbitrary a, CoArbitrary a) => Arbitrary (ReproduceArgs a) where
    arbitrary = do
        p <- arbitrary `suchThat` (\x -> x >= 0 && x <= 1)
        n <- arbitrary `suchThat` (> 1)
        agents <- newPop n <$> arbitrary
        -- generate a fitness fn that preserves list length and produces fitness > 0
        f <- arbitrary `suchThat` (\f -> all (\xs -> length xs == 2
                                                  && all (> 0) xs)
                                             (map (f . map agentChromosome)
                                                  (matchups2 agents)))
        return $ ReproduceArgs (p, f, agents)


newPop :: Int -> Agent a -> [Agent a]
newPop n Agent{..} = newPopulation n agentEncoder agentDecoder agentChromosome


prop_mergeAgentsUnique :: [Agent a] -> Bool
prop_mergeAgentsUnique = unique . mergeAgents

prop_mergeAgentsKeepAll :: [Agent a] -> Bool
prop_mergeAgentsKeepAll xs = all (`elem` mergeAgents xs) xs

prop_mergeAgentsNoNew :: [Agent a] -> Bool
prop_mergeAgentsNoNew xs = all (`elem` xs) (mergeAgents xs)

prop_newPopulationLength :: NonNegative Int -> Agent a -> Bool
prop_newPopulationLength n agent = length pop == getNonNegative n
    where pop = newPop (getNonNegative n) agent

prop_newPopulationIds :: NonNegative Int -> Agent a -> Bool
prop_newPopulationIds n agent = and $ zipWith (==) (map agentId pop) (iterate (+1) 0)
    where pop = newPop (getNonNegative n) agent

prop_newPopulationUniform :: Eq a => NonNegative Int -> Agent a -> Bool
prop_newPopulationUniform n agent = uniform agentCommon pop True
    where pop = newPop (getNonNegative n) agent
          uniform _ [] acc = acc
          uniform f (x:xs) acc = uniform f xs (acc && all (f x) xs)
          agentCommon x y = fEq c && fEq enc && fEq dec
              where fEq f = f x == f y
                    recordApply f g z = f z $ g z
                    c = agentChromosome
                    enc = recordApply agentEncoder c
                    dec = recordApply agentDecoder enc

prop_reproduceLength :: ReproduceArgs a -> Gen Bool
prop_reproduceLength (ReproduceArgs (p, f, pop)) =
    (== length pop) . length <$> reproduce p f matchups2 pop

prop_reproduceIds :: ReproduceArgs a -> Gen Bool
prop_reproduceIds (ReproduceArgs (p, f, pop)) =
    fIncreasing True agentId <$> reproduce p f matchups2 pop
    where fIncreasing acc _ [] = acc
          fIncreasing acc _ [_] = acc
          fIncreasing acc g (x:y:xs) = fIncreasing (acc && g x < g y) g (y:xs)


return []
runTests :: IO Bool
runTests = $quickCheckAll
