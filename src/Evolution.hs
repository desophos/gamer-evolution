{-# LANGUAGE RecordWildCards, DeriveGeneric, ScopedTypeVariables #-}

module Evolution
    ( Agent(..)
    , EvolutionParams(..)
    , newPopulation
    , mutate
    , reproduce
    , evolve
    , mergeAgents
    , getFitness
    ) where

import           Data.List                      ( sort
                                                , sortOn
                                                )
import           GHC.Float.RealFracMethods      ( floorDoubleInt
                                                , roundDoubleInt
                                                )
import           GHC.Generics                   ( Generic )
import           Test.Invariant                 ( inverts )
import           Test.QuickCheck                ( Arbitrary(arbitrary)
                                                , CoArbitrary
                                                , Gen
                                                , Positive
                                                , choose
                                                , chooseInt
                                                , elements
                                                , frequency
                                                , suchThat
                                                , vectorOf
                                                )
import           Util                           ( combineWith
                                                , mergeAll
                                                , omit
                                                )


data EvolutionParams = EvolutionParams
    { evolveGenerations :: !Int -- ^ Number of generations to evolve the population for.
    , evolvePopSize     :: !Int -- ^ Number of Agents in the population.
    , evolveSurvivors   :: !Int -- ^ Number of Agents that survive each generation (>= 2).
    , evolveMutateP     :: !Double -- ^ Probability of mutation per gene (0 <= p <= 1).
    }
    deriving (Eq, Show)

instance Arbitrary EvolutionParams where
    arbitrary = do
        evolveGenerations <- chooseInt (5, 10)
        evolvePopSize     <- chooseInt (20, 60) `suchThat` even
        let both f (x, y) = (f x, f y)
            scalePopSize = roundDoubleInt . (* fromIntegral evolvePopSize)
            surviveRange = both scalePopSize (0.3, 0.8)
        evolveSurvivors <- arbitrary `suchThat` combineWith
            (&&)
            [ (> 1)
            , (< evolvePopSize)
            , (> fst surviveRange)
            , (< snd surviveRange)
            ]
        evolveMutateP <- choose (0, 0.1) `suchThat` (> 0) -- avoid division by zero in analysis
        return EvolutionParams { .. }

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
-- produced via a simulation of genetic crossover and mutation.
crossover :: EvolutionParams -> Agent a -> Agent a -> Gen (Agent a)
crossover EvolutionParams {..} x y = do
    let encoder   = agentEncoder x
        decoder   = agentDecoder x
        genes     = agentGenes x
        encoded   = encoder . agentGenome
        part1     = flip take (encoded x)
        part2     = flip drop (encoded y)
        combineAt = combineWith (++) [part1, part2]
    crosspoint <- chooseInt (1, length (encoded x) - 1)
    mutated    <- mutate evolveMutateP genes (combineAt crosspoint)
    return $ newAgent encoder decoder genes (decoder mutated)

reproduce
    :: EvolutionParams
    -> ([a] -> [Int]) -- ^ Fitness function to determine Agents' likelihood to reproduce. Must preserve list length.
    -> ([Agent a] -> [[Agent a]]) -- ^ Given the population, produces the list of groups passed to the fitness fn.
    -> [Agent a] -- ^ The population of Agents to reproduce.
    -> Gen [Agent a] -- ^ The population with non-surviving Agents replaced by children produced by genetic crossover.
reproduce params@EvolutionParams {..} f matchup pop = (survived ++) <$> births
  where
    survived =
        sortOn agentId
            . take evolveSurvivors
            . sortOn agentFitness
            . getFitness f
            . matchup
            $ pop
    fitPairs xs = zip (map agentFitness xs) (map pure xs)
    pickFit = frequency . fitPairs
    mate    = do
        x <- pickFit survived
        y <- pickFit $ survived `omit` x
        crossover params x y
    nBirth  = length pop - evolveSurvivors
    nextIds = iterate (+ 1) . (+ 1) . agentId . last $ survived
    births  = flip (zipWith withId) nextIds . replicate nBirth <$> mate

-- | Evolves a population over a number of generations. See `reproduce`.
evolve
    :: EvolutionParams
    -> ([a] -> [Int])
    -> ([Agent a] -> [[Agent a]])
    -> [Agent a]
    -> Gen [Agent a]
evolve params@EvolutionParams {..} f matchup pop = evolve'
    (pure pop)
    evolveGenerations
  where
    reproduce' = reproduce params f matchup
    evolve' xs n = if n <= 0 then xs else evolve' (reproduce' =<< xs) (n - 1)
