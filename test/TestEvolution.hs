{-# LANGUAGE TemplateHaskell, RecordWildCards, ScopedTypeVariables #-}
module TestEvolution where

import           Data.Typeable                  ( Typeable )
import           Evolution                      ( Agent(..)
                                                , evolve
                                                , mergeAgents
                                                , newPopulation
                                                , reproduce
                                                )
import           Instances                      ( )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , CoArbitrary
                                                , Gen
                                                , NonNegative(getNonNegative)
                                                , NonPositive(getNonPositive)
                                                , Positive(getPositive)
                                                , choose
                                                , quickCheckAll
                                                , suchThat
                                                )
import           Util                           ( matchups2
                                                , unique
                                                )


newtype ReproduceArgs a = ReproduceArgs (Double, [a] -> [Int], [Agent a]) deriving (Show)

instance (Show a, Typeable a, Eq a, Arbitrary a, CoArbitrary a) => Arbitrary (ReproduceArgs a) where
    arbitrary = do
        p      <- choose (0, 1)
        n      <- arbitrary `suchThat` (> 1)
        agents <- newPop n =<< arbitrary
        -- generate a fitness fn that strictly increases fitness
        -- fitness starts at 0 so this also guarantees fitness will be > 0
        -- necessary because fitnesses are passed as weights to `frequency`
        f      <- (arbitrary :: Gen (a -> Positive Int))
        return $ ReproduceArgs (p, map (getPositive . f), agents)


newPop :: (Arbitrary a) => Int -> Agent a -> Gen [Agent a]
newPop n Agent {..} = newPopulation n agentEncoder agentDecoder arbitrary

prop_mergeAgentsUnique :: [Agent a] -> Bool
prop_mergeAgentsUnique = unique . mergeAgents

prop_mergeAgentsKeepAll :: [Agent a] -> Bool
prop_mergeAgentsKeepAll xs = all (`elem` mergeAgents xs) xs

prop_mergeAgentsNoNew :: [Agent a] -> Bool
prop_mergeAgentsNoNew xs = all (`elem` xs) (mergeAgents xs)

prop_newPopulationLength
    :: Arbitrary a => NonNegative Int -> Agent a -> Gen Bool
prop_newPopulationLength n agent = do
    pop <- newPop (getNonNegative n) agent
    return $ length pop == getNonNegative n

prop_newPopulationIds :: Arbitrary a => NonNegative Int -> Agent a -> Gen Bool
prop_newPopulationIds n agent = do
    pop <- newPop (getNonNegative n) agent
    return . and $ zipWith (==) (map agentId pop) (iterate (+ 1) 0)

prop_newPopulationUniform
    :: (Eq a, Arbitrary a) => NonNegative Int -> Agent a -> a -> Gen Bool
prop_newPopulationUniform n agent c = do
    pop <- newPop (getNonNegative n) agent
    return $ uniform agentCommon pop True
  where
    uniform _ []       acc = acc
    uniform f (x : xs) acc = uniform f xs (acc && all (f x) xs)
    agentCommon x y = fEq enc && fEq dec
      where
        fEq f = f x == f y
        enc z = agentEncoder z c
        dec z = agentDecoder z $ enc z

prop_reproduceLength :: ReproduceArgs a -> Gen Bool
prop_reproduceLength (ReproduceArgs (p, f, pop)) = do
    pop' <- reproduce p f matchups2 pop
    return $ length pop == length pop'

prop_reproduceIds :: ReproduceArgs a -> Gen Bool
prop_reproduceIds (ReproduceArgs (p, f, pop)) =
    fIncreasing True agentId <$> reproduce p f matchups2 pop
  where
    fIncreasing acc _ []           = acc
    fIncreasing acc _ [_         ] = acc
    fIncreasing acc g (x : y : xs) = fIncreasing (acc && g x < g y) g (y : xs)

prop_evolveId :: ReproduceArgs a -> NonPositive Int -> Gen Bool
prop_evolveId (ReproduceArgs (p, f, pop)) n =
    (pop ==) <$> evolve p f matchups2 pop (getNonPositive n)

prop_evolvePreserve :: ReproduceArgs a -> Positive Int -> Gen Bool
prop_evolvePreserve (ReproduceArgs (p, f, pop)) n = do
    pop' <- evolve p f matchups2 pop (getPositive n)
    let args' = ReproduceArgs (p, f, pop')
    len <- prop_reproduceLength args'
    ids <- prop_reproduceIds args'
    return $ len && ids


return []
runTests :: IO Bool
runTests = $quickCheckAll
