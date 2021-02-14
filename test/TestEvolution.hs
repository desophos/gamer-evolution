{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module TestEvolution where

import Test.QuickCheck
import Test.QuickCheck.All()
import Evolution
import Util
import Instances()


newtype ReproduceArgs a = ReproduceArgs (Double, [a] -> [Int], [Agent a]) deriving (Show)

instance (Eq a, Arbitrary a, CoArbitrary a) => Arbitrary (ReproduceArgs a) where
    arbitrary = do
        p <- arbitrary `suchThat` (\x -> x >= 0 && x <= 1)
        n <- arbitrary `suchThat` (> 1)
        agents <- newPop n <$> arbitrary
        f <- arbitrary `suchThat` (\f -> all ((== 2) . length)
                                             (map (f . map agentChromosome)
                                                  (matchups2 agents)))
        return $ ReproduceArgs (p, f, agents)


prop_mergeAgentsUnique :: [Agent a] -> Bool
prop_mergeAgentsUnique = unique . mergeAgents

prop_mergeAgentsKeepAll :: [Agent a] -> Bool
prop_mergeAgentsKeepAll xs = all (`elem` mergeAgents xs) xs

prop_mergeAgentsNoNew :: [Agent a] -> Bool
prop_mergeAgentsNoNew xs = all (`elem` xs) (mergeAgents xs)

newPop :: Int -> Agent a -> [Agent a]
newPop n Agent{..} = newPopulation n agentEncoder agentDecoder agentChromosome

prop_newPopulationLength :: NonNegative Int -> Agent a -> Bool
prop_newPopulationLength n agent = length pop == getNonNegative n
    where pop = newPop (getNonNegative n) agent

prop_newPopulationIds :: NonNegative Int -> Agent a -> Bool
prop_newPopulationIds n agent = and $ zipWith (==) (map agentId pop) (iterate (+1) 0)
    where pop = newPop (getNonNegative n) agent

prop_newPopulationUniform :: Eq a => NonNegative Int -> Agent a -> Bool
prop_newPopulationUniform n agent = uniform agentCommon pop
    where pop = newPop (getNonNegative n) agent
          uniform :: (a -> a -> Bool) -> [a] -> Bool
          uniform _ [] = True
          uniform f (x:xs) = all (f x) xs && uniform f xs
          agentCommon :: Eq a => Agent a -> Agent a -> Bool
          agentCommon x y = fEq c && fEq enc && fEq dec
              where fEq f = f x == f y
                    recordApply :: (a -> (b -> c)) -> (a -> b) -> a -> c
                    recordApply f g z = f z $ g z
                    c = agentChromosome
                    enc = recordApply agentEncoder c
                    dec = recordApply agentDecoder enc

prop_reproduceLength :: ReproduceArgs a -> Gen Bool
prop_reproduceLength (ReproduceArgs (p, f, pop)) =
    (== length pop) . length <$> reproduce p f matchups2 pop

prop_reproduceIds :: ReproduceArgs a -> Gen Bool
prop_reproduceIds (ReproduceArgs (p, f, pop)) =
    fIncreasing agentId <$> reproduce p f matchups2 pop
    where fIncreasing :: Ord b => (a -> b) -> [a] -> Bool
          fIncreasing _ [] = True
          fIncreasing _ [_] = True
          fIncreasing g (x:y:xs) = g x < g y && fIncreasing g (y:xs)


return []
runTests :: IO Bool
runTests = $quickCheckAll
