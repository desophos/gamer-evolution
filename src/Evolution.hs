{-# LANGUAGE RecordWildCards, DeriveGeneric, ScopedTypeVariables #-}

module Evolution
    ( Agent(..)
    , newPopulation
    , reproduce
    , evolve
    , mergeAgents
    , getFitness
    ) where

import           Data.List                      ( sort
                                                , sortOn
                                                )
import           GHC.Float.RealFracMethods      ( floorDoubleInt )
import           GHC.Generics                   ( Generic )
import           Test.Invariant                 ( inverts )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , CoArbitrary
                                                , Gen
                                                , choose
                                                , frequency
                                                , suchThat
                                                , vectorOf
                                                )
import           Util                           ( combineWith
                                                , mergeAll
                                                , omit
                                                )


data Agent a = Agent
    { agentId      :: !Int
    , agentFitness :: !Int
    , agentGenome  :: !a
    , agentGenes   :: ![Char] -- ^ The possible characters that make up the encoded genome. Length should be >= 2.
    , agentEncoder :: !(a -> String)
    , agentDecoder :: !(String -> a)
    }
    deriving Generic

instance Eq (Agent a) where
    x == y = agentId x == agentId y
instance Ord (Agent a) where
    x <= y = agentId x <= agentId y
instance (Eq a, Arbitrary a, CoArbitrary a) => Arbitrary (Agent a) where
    arbitrary = do
        let flip3 f x y z = f z y x
            agentId      = 0
            agentFitness = 0
        agentGenome  <- arbitrary
        agentGenes   <- arbitrary `suchThat` ((> 1) . length)
        agentEncoder <-
            arbitrary `suchThat` (\f -> all (`elem` agentGenes) (f agentGenome))
        agentDecoder <-
            arbitrary `suchThat` flip3 inverts agentGenome agentEncoder
        return Agent { .. }


-- trivial helper functions
withId :: Agent a -> Int -> Agent a
withId x i = x { agentId = i }

withFitness :: Agent a -> Int -> Agent a
withFitness x i = x { agentFitness = i }


-- | Merges agents by ID, combining fitness.
mergeAgents :: [Agent a] -> [Agent a]
mergeAgents = mergeAll f . sort
    where f x y = x { agentFitness = agentFitness x + agentFitness y }

-- | Runs a fitness function on a population of agents.
getFitness
    :: ([a] -> [Int]) -- ^ Fitness function.
    -> [[Agent a]] -- ^ Agent "matchups" to pass to the fitness function.
    -> [Agent a] -- ^ The population with updated agentFitness.
getFitness f = mergeAgents . concatMap applyScores
  where
    getScores = f . map agentGenome
    applyScores xs = zipWith withFitness xs (getScores xs)


newAgent :: (a -> String) -> (String -> a) -> [Char] -> a -> Agent a
newAgent agentEncoder agentDecoder agentGenes agentGenome = Agent { .. }
  where
    agentId      = 0
    agentFitness = 0

-- | Returns a population of agents with incremental IDs.
newPopulation
    :: Int -- ^ Number of agents to generate.
    -> [Char] -- ^ Possible genes that make up the encoded genome.
    -> (a -> String) -- ^ Encodes an agent's genome.
    -> (String -> a) -- ^ Decodes an agent's genome. Should invert the encoder.
    -> Gen a -- ^ Genome generator.
    -> Gen [Agent a]
newPopulation n genes enc dec genome = do
    let agent = newAgent enc dec genes <$> genome
    agents <- vectorOf n agent
    return $ zipWith withId agents (iterate (+ 1) 0)


mutate
    :: Double -- ^ Probability per gene of mutation.
    -> [Char] -- ^ Possible genes to mutate into.
    -> String -- ^ The genome to mutate.
    -> Gen String
mutate pMutate opts = f
  where
    f []       = pure []
    f (x : xs) = do
        p   <- choose (0, 1)
        x'  <- if p <= pMutate then elements (opts `omit` x) else pure x
        xs' <- f xs
        return (x' : xs')

-- | Given 2 parent Agents, returns a child Agent whose chromosome is
-- produced via a simulation of genetic crossover.
crossover :: Agent a -> Agent a -> Gen (Agent a)
crossover x y = do
    let encoder = agentEncoder x
        decoder = agentDecoder x
        c       = encoder . agentGenome
        part1   = flip take (c x)
        part2   = flip drop (c y)
        newC    = decoder . combineWith (++) [part1, part2]
    crosspoint <- choose (1, length (c x) - 1)
    return $ newAgent encoder decoder (newC crosspoint)

reproduce
    :: Double -- ^ Proportion (0 < p < 1) of the population to survive to the next generation (minimum 2 members).
    -> ([a] -> [Int]) -- ^ Fitness function to determine Agents' likelihood to reproduce. Must preserve list length.
    -> ([Agent a] -> [[Agent a]]) -- ^ Given the population, produces the list of groups passed to the fitness fn.
    -> [Agent a] -- ^ The population of Agents to reproduce.
    -> Gen [Agent a] -- ^ The population with non-surviving Agents replaced by children produced by genetic crossover.
reproduce pSurvive f matchup pop = (survived ++) <$> births
  where
    nSurvive =
        max 2 . floorDoubleInt . (*) pSurvive . fromIntegral . length $ pop
    survived =
        sortOn agentId
            . take nSurvive
            . sortOn agentFitness
            . getFitness f
            . matchup
            $ pop
    fitPairs xs = zip (map agentFitness xs) (map pure xs)
    pickFit = frequency . fitPairs
    mate = do
        x <- pickFit survived
        y <- pickFit $ survived `omit` x
        crossover x y
    nBirth  = length pop - nSurvive
    nextIds = iterate (+ 1) . (+ 1) . agentId . last $ survived
    births  = flip (zipWith withId) nextIds . replicate nBirth <$> mate

-- | Evolves a population over a number of generations. See `reproduce`.
evolve
    :: Double
    -> ([a] -> [Int])
    -> ([Agent a] -> [[Agent a]])
    -> [Agent a]
    -> Int -- ^ The number of generations.
    -> Gen [Agent a]
evolve pSurvive f matchup pop = evolve' (pure pop)
  where
    reproduce' = reproduce pSurvive f matchup
    evolve' xs n = if n <= 0 then xs else evolve' (reproduce' =<< xs) (n - 1)
