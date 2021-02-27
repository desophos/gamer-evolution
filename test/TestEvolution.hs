{-# LANGUAGE TemplateHaskell, RecordWildCards, ScopedTypeVariables #-}
module TestEvolution where

import qualified Data.Set                      as S
import           Data.Typeable                  ( Typeable )
import           Evolution                      ( Agent(..)
                                                , EvolutionParams(..)
                                                , evolve
                                                , mergeAgents
                                                , mutate
                                                , newPopulation
                                                , reproduce
                                                )
import           Instances                      ( )
import           Statistics.ConfidenceInt       ( binomialCI )
import           Statistics.Types               ( ConfInt(ConfInt)
                                                , Estimate(Estimate)
                                                , cl95
                                                )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , CoArbitrary
                                                , Gen
                                                , NonNegative(getNonNegative)
                                                , NonPositive(getNonPositive)
                                                , Positive(getPositive)
                                                , Property
                                                , choose
                                                , cover
                                                , elements
                                                , listOf1
                                                , quickCheckAll
                                                , suchThat
                                                , vectorOf
                                                )
import           Util                           ( matchups
                                                , unique
                                                )


newtype ReproduceArgs a = ReproduceArgs (EvolutionParams, [a] -> [Int], [Agent a]) deriving (Show)

instance (Show a, Typeable a, Eq a, Arbitrary a, CoArbitrary a) => Arbitrary (ReproduceArgs a) where
    arbitrary = do
        params@EvolutionParams {..} <- arbitrary
        agents                      <- newPop evolvePopSize =<< arbitrary
        -- generate a fitness fn that strictly increases fitness
        -- fitness starts at 0 so this also guarantees fitness will be > 0
        -- necessary because fitnesses are passed as weights to `frequency`
        f                           <- (arbitrary :: Gen (a -> Positive Int))
        return $ ReproduceArgs (params, map (getPositive . f), agents)

newtype MutateArgs = MutateArgs (Double, [Char], String) deriving (Show, Eq)

instance Arbitrary MutateArgs where
    arbitrary = do
        p      <- choose (0, 1)
        genes  <- listOf1 arbitrary `suchThat` ((> 1) . length)
        genome <- vectorOf 1000 (elements genes)
        return $ MutateArgs (p, genes, genome)


newPop :: (Arbitrary a) => Int -> Agent a -> Gen [Agent a]
newPop n Agent {..} =
    newPopulation n agentGenes agentEncoder agentDecoder arbitrary

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
    return $ uniform pop True
  where
    uniform []       acc = acc
    uniform (x : xs) acc = uniform xs (acc && all (agentCommon x) xs)
    agentCommon x y = fEq enc && fEq dec
      where
        fEq f = f x == f y
        enc z = agentEncoder z c
        dec z = agentDecoder z $ enc z

prop_mutate :: MutateArgs -> Gen Property
prop_mutate (MutateArgs (p, genes, genome)) = do
    mutated <- mutate p genes genome
    let Estimate p' (ConfInt lower upper _) = binomialCI
            cl95
            (length genome)
            (length $ filter (uncurry (/=)) (zip genome mutated))
    -- proportion of mutations == p with 95% confidence
    return $ cover 95 ((p > p' - lower) && (p < p' + upper)) "near p" True

prop_reproduceLength :: ReproduceArgs a -> Gen Bool
prop_reproduceLength (ReproduceArgs (params, f, pop)) = do
    pop' <- reproduce params f (matchups 2 . S.fromDistinctAscList) pop
    return $ length pop == length pop'

prop_reproduceIds :: ReproduceArgs a -> Gen Bool
prop_reproduceIds (ReproduceArgs (params, f, pop)) =
    fIncreasing True agentId
        <$> reproduce params f (matchups 2 . S.fromDistinctAscList) pop
  where
    fIncreasing acc _ []           = acc
    fIncreasing acc _ [_         ] = acc
    fIncreasing acc g (x : y : xs) = fIncreasing (acc && g x < g y) g (y : xs)

prop_evolveId :: ReproduceArgs a -> NonPositive Int -> Gen Bool
prop_evolveId (ReproduceArgs (params, f, pop)) n =
    (pop ==)
        <$> evolve params { evolveGenerations = getNonPositive n }
                   f
                   (matchups 2 . S.fromDistinctAscList)
                   pop

prop_evolvePreserve :: ReproduceArgs a -> Gen Bool
prop_evolvePreserve (ReproduceArgs (params, f, pop)) = do
    pop' <- evolve params f (matchups 2 . S.fromDistinctAscList) pop
    let args' = ReproduceArgs (params, f, pop')
    len <- prop_reproduceLength args'
    ids <- prop_reproduceIds args'
    return $ len && ids


return []
runTests :: IO Bool
runTests = $quickCheckAll
