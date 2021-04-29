{-# LANGUAGE TemplateHaskell, RecordWildCards, ScopedTypeVariables #-}
module TestEvolution where

import qualified Data.ByteString.Lazy          as B
import           Data.MonoTraversable           ( MonoFoldable(olength) )
import qualified Data.Set                      as S
import           Data.Typeable                  ( Typeable )
import           Data.Word                      ( Word8 )
import           Evolution                      ( Agent(..)
                                                , EvolutionParams(..)
                                                , evolve
                                                , genPopulation
                                                , mergeAgents
                                                , mutate
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
import           Util                           ( combineWith
                                                , matchups
                                                , unique
                                                )


newtype ReproduceArgs a = ReproduceArgs (EvolutionParams, [a] -> [Float], [Agent a]) deriving (Show)

instance (Show a, Typeable a, Eq a, Arbitrary a, CoArbitrary a) => Arbitrary (ReproduceArgs a) where
    arbitrary = do
        params@EvolutionParams {..} <- arbitrary
        agents                      <- genPop evolvePopSize =<< arbitrary
        -- generate a fitness fn that strictly increases fitness
        -- fitness starts at 0 so this also guarantees fitness will be > 0
        -- necessary because fitnesses are passed as weights to `frequency`
        f                           <- (arbitrary :: Gen (a -> Positive Float))
        return $ ReproduceArgs (params, map (getPositive . f), agents)

newtype MutateArgs = MutateArgs (Double, [Word8], B.ByteString) deriving (Show, Eq)

instance Arbitrary MutateArgs where
    arbitrary = do
        p     <- choose (0, 1)
        genes <- listOf1 arbitrary
            `suchThat` combineWith (&&) [unique, (> 1) . length]
        genome <- B.pack <$> vectorOf 1000 (elements genes)
        return $ MutateArgs (p, genes, genome)


genPop :: (Arbitrary a) => Int -> Agent a -> Gen [Agent a]
genPop n Agent {..} =
    genPopulation n agentGenes agentEncoder agentDecoder arbitrary

prop_mergeAgentsUnique :: [Agent a] -> Bool
prop_mergeAgentsUnique = unique . mergeAgents

prop_mergeAgentsKeepAll :: [Agent a] -> Bool
prop_mergeAgentsKeepAll xs = all (`elem` mergeAgents xs) xs

prop_mergeAgentsNoNew :: [Agent a] -> Bool
prop_mergeAgentsNoNew xs = all (`elem` xs) (mergeAgents xs)

prop_populationLength :: Arbitrary a => NonNegative Int -> Agent a -> Gen Bool
prop_populationLength n agent = do
    pop <- genPop (getNonNegative n) agent
    return $ length pop == getNonNegative n

prop_populationIds :: Arbitrary a => NonNegative Int -> Agent a -> Gen Bool
prop_populationIds n agent = do
    pop <- genPop (getNonNegative n) agent
    return . and $ zipWith (==) (map agentId pop) [0 ..]

prop_populationUniform
    :: (Eq a, Arbitrary a) => NonNegative Int -> Agent a -> a -> Gen Bool
prop_populationUniform n agent c = do
    pop <- genPop (getNonNegative n) agent
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
            (olength genome)
            (olength $ filter id (B.zipWith (/=) genome mutated))
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
