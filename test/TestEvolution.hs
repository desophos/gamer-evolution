{-# LANGUAGE TemplateHaskell, RecordWildCards, ScopedTypeVariables #-}
module TestEvolution where

import qualified Data.ByteString.Lazy          as B
import           Data.MonoTraversable           ( MonoFoldable(olength) )
import           Data.Ord                       ( Down(Down) )
import qualified Data.Set                      as S
import           Data.Typeable                  ( Typeable )
import           Data.Word                      ( Word8 )
import           Instances                      ( )
import           Internal.Evolution
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
import           Util


newtype ReproduceArgs = ReproduceArgs (EvolutionParams, [[Word8]] -> [Float], [Agent [Word8]]) deriving (Show)

instance Arbitrary ReproduceArgs where
    arbitrary = do
        params@EvolutionParams {..} <- arbitrary
        agents <- genPop evolvePopSize =<< arbitrary
        -- generate a fitness fn that strictly increases fitness
        -- fitness starts at 0 so this also guarantees fitness will be > 0
        -- necessary because fitnesses are passed as weights to `frequency`
        f <- (arbitrary :: Gen ([Word8] -> Positive Float))
        return $ ReproduceArgs (params, map (getPositive . f), agents)

newtype CrossoverArgs = CrossoverArgs (EvolutionParams, Agent [Word8], Agent [Word8]) deriving (Show)

instance Arbitrary CrossoverArgs where
    arbitrary = do
        params <- arbitrary
        agent1 <- arbitrary
        agent2 <- genAgentLike agent1
        return $ CrossoverArgs (params, agent1, agent2)

newtype MutateArgs = MutateArgs (Double, [Word8], B.ByteString) deriving (Show, Eq)

instance Arbitrary MutateArgs where
    arbitrary = do
        p     <- choose (0, 1)
        genes <- listOf1 arbitrary
            `suchThat` combineWith (&&) [unique, (> 1) . length]
        genome <- B.pack <$> vectorOf 1000 (elements genes)
        return $ MutateArgs (p, genes, genome)


matchup :: [Agent a] -> [[Agent a]]
--matchup = matchWithBest 2 10 (Down . agentFitness) . S.fromDistinctAscList
matchup = matchups 2 . S.fromDistinctAscList


genPop :: Int -> Agent [Word8] -> Gen [Agent [Word8]]
genPop n Agent {..} =
    genPopulation n agentGenes agentEncoder agentDecoder arbitrary


prop_mergeAgentsRemovesDuplicates :: [Agent [Word8]] -> Bool
prop_mergeAgentsRemovesDuplicates = unique . mergeAgents


prop_mergeAgentsDoesntMissAny :: [Agent [Word8]] -> Bool
prop_mergeAgentsDoesntMissAny xs = all (`elem` mergeAgents xs) xs


prop_mergeAgentsDoesntCreateAny :: [Agent [Word8]] -> Bool
prop_mergeAgentsDoesntCreateAny xs = all (`elem` xs) (mergeAgents xs)


prop_populationIsCorrectSize :: NonNegative Int -> Agent [Word8] -> Gen Bool
prop_populationIsCorrectSize n agent = do
    pop <- genPop (getNonNegative n) agent
    return $ length pop == getNonNegative n


prop_populationHasCorrectIds :: NonNegative Int -> Agent [Word8] -> Gen Bool
prop_populationHasCorrectIds n agent = do
    pop <- genPop (getNonNegative n) agent
    return . and $ zipWith (==) (map agentId pop) [0 ..]


prop_populationHasSameEncoderDecoderGenes
    :: NonNegative Int -> Agent [Word8] -> [Word8] -> Gen Bool
prop_populationHasSameEncoderDecoderGenes n agent c = do
    pop <- genPop (getNonNegative n) agent
    return $ uniform pop True
  where
    uniform []       acc = acc
    uniform (x : xs) acc = uniform xs (acc && all (agentCommon x) xs)
    agentCommon x y = fEq enc && fEq dec && fEq agentGenes
      where
        fEq f = f x == f y
        enc z = agentEncoder z c
        dec z = agentDecoder z $ enc z


prop_mutateProducesCorrectProportion :: MutateArgs -> Gen Property
prop_mutateProducesCorrectProportion (MutateArgs (p, genes, genome)) = do
    mutated <- mutate p genes genome
    let Estimate p' (ConfInt lower upper _) = binomialCI
            cl95
            (olength genome)
            (olength $ filter id (B.zipWith (/=) genome mutated))
    -- 90% of the time, proportion of mutations == p with 95% confidence
    return $ cover 90 ((p > p' - lower) && (p < p' + upper)) "near p" True

-- | Crossover should preserve genome length.
prop_crossoverPreservesLength :: CrossoverArgs -> Gen Bool
prop_crossoverPreservesLength (CrossoverArgs (params, a1, a2)) = do
    a' <- crossover params a1 a2
    let genomeLengthA' = genomeLength a'
    return
        $  genomeLengthA'
        == genomeLength a1
        && genomeLengthA'
        == genomeLength a2
    where genomeLength = B.length . encodeGenome

-- | Crossover should produce a genome that starts with the first gene from the first parent and ends with the last gene from the second parent.
prop_crossoverIsHalfAndHalf :: CrossoverArgs -> Gen Bool
prop_crossoverIsHalfAndHalf (CrossoverArgs (params, a1, a2)) = do
    a' <- crossover params a1 a2
    let genome' = encodeGenome a'
    return
        $  B.head genome'
        == B.head (encodeGenome a1)
        && B.last genome'
        == B.last (encodeGenome a2)


-- | Reproduction should preserve population size.
prop_reproducePreservesPopSize :: ReproduceArgs -> Gen Bool
prop_reproducePreservesPopSize (ReproduceArgs (params, f, pop)) = do
    pop' <- reproduce params f matchup pop
    return $ length pop == length pop'


-- | Population IDs should stay in increasing order after reproduction.
prop_reproducePreservesPopIds :: ReproduceArgs -> Gen Bool
prop_reproducePreservesPopIds (ReproduceArgs (params, f, pop)) =
    fIncreasing True agentId <$> reproduce params f matchup pop
  where
    fIncreasing acc _ []           = acc
    fIncreasing acc _ [_         ] = acc
    fIncreasing acc g (x : y : xs) = fIncreasing (acc && g x < g y) g (y : xs)


-- | A non-positive number of generations should return the population unchanged.
prop_evolveInvalidGenerationsIsNoOp
    :: ReproduceArgs -> NonPositive Int -> Gen Bool
prop_evolveInvalidGenerationsIsNoOp (ReproduceArgs (params, f, pop)) n = do
    pop' <- evolve params { evolveGenerations = getNonPositive n } f matchup pop
    return $ pop == pop'


-- | Population size and ID sorting should be preserved with evolution.
prop_evolvePreservesPop :: ReproduceArgs -> Gen Bool
prop_evolvePreservesPop (ReproduceArgs (params, f, pop)) = do
    pop' <- evolve params f matchup pop
    let args' = ReproduceArgs (params, f, pop')
    len <- prop_reproducePreservesPopSize args'
    ids <- prop_reproducePreservesPopIds args'
    return $ len && ids


return []
runTests :: IO Bool
runTests = $quickCheckAll
